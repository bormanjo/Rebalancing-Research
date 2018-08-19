library(xts)
library(quantmod)
library(magrittr)
library(dygraphs)
library(lubridate)
library(quadprog)



# Functions ---------------------------------------------------------------

variance <- function(weights, cov){
  weights <- as.numeric(weights)
  as.numeric(t(weights) %*% cov %*% weights)
}

expected_utility <- function(mu, sigma){
  as.numeric(log10(1 + mu) - ( sigma / (2 * (1 + mu)^2)))
}

get_state_space <- function(n, w_min, w_max, w_incr){
  require(RcppAlgos)
  vec <- seq(w_min, w_max, w_incr)
  permuteGeneral(vec, n, repetition = TRUE, constraintFun = "sum", comparisonFun = "==", limitConstraints = 1)
}

CEC <- function(opt_EU, EU, capital){
  as.numeric((exp(opt_EU) - exp(EU)) * capital)
}

TC <- function(w_opt, w, tc){
  w_opt %<>% as.numeric()
  w %<>% as.numeric()
  tc %<>% as.numeric()
  sum(tc * abs(w_opt - w))
}

covariance_t <- function(rets, date_start, lookback = years(3)){
  cov(rets[paste0(date_start - lookback, "/", date_start)])
}

# Optimal portfolio
get_opt_weights <- function(cov_t){
  w_opt <- solve(cov_t) %*% rep(1, ncol(cov_t))
  w_opt[,1] / sum(w_opt)
}

# Parameters --------------------------------------------------------------------

tickers <- c("XLY", "XLP", "XLE", "XLF", "XLI", "XLB", "XLK", "XLU") # Omit XLC and XLRE
start_years <- 3
costs <- c(40, 50, 60)
capital <- 1e7

w_min <- 0.0
w_max <- 0.20
w_incr <- 0.05

# Data --------------------------------------------------------------------


getSymbols(tickers)

pxs <- NULL
rets <- NULL

for(ticker in tickers){
  pxs <- ticker %>% get %>% Ad %>% merge(pxs)
  rets <- ticker %>% get %>% dailyReturn %>% merge(rets)
}

colnames(pxs) <- tickers
colnames(rets) <- tickers

# Preview Data
plot(pxs)
plot(rets)

date_start <- pxs %>% index %>% first + years(start_years)

cov_start <- cov(rets[paste0("/", date_start)]) * 100

temp <- rets[paste0(date_start,"/")]

template <- xts(x = matrix(data = 0, nrow = nrow(temp), ncol = length(tickers)), order.by = index(temp))
rm(temp)

colnames(template) <- tickers

# Optimal Portfolio -------------------------------------------------------

# Minimum Variance
opt_w <- get_opt_weights(cov_start)

# Dynamic Rebalancing -----------------------------------------------------
  
# Portfolio Data

## Holdings Information
weights <- template
investments <- template

weights[1,] <- opt_w
investments[1,] <- weights[1,] * capital

## Cost and Statistics Information
cnames <- c("return", "var", "eu", "opt_var", "opt_eu", "tc", "cec", "costs", "rebalance")
portfolio_data <- xts(x = matrix(data = 0, nrow = nrow(weights), ncol = length(cnames)),
                      order.by = index(weights))
colnames(portfolio_data) <- cnames

## Weight to rebalance
min_cost_weights <- template

# State Space Data

## Holdings Information
s_weights <- get_state_space(ncol(weights), w_min = w_min, w_max = w_max, w_incr = w_incr) %>% as.data.frame()
s_investments <- matrix(data = 0, nrow = nrow(s_weights), ncol = ncol(s_weights)) %>% as.data.frame()

## Cost and Statistics Information
cnames <- c("return", "var", "eu", "tc", "cec", "costs")
s_portfolio_data <- as.data.frame(matrix(data = 0, nrow = nrow(s_weights), ncol = length(cnames)))

colnames(s_weights) <- tickers
colnames(s_investments) <- tickers
colnames(s_portfolio_data) <- cnames

pb <- txtProgressBar(min = 2, max = nrow(weights), style = 3)

# Loop through time
for(t1 in 2:(nrow(weights))){
  # Calculate daily portfolio movements
  current_date <- index(weights)[t1]
  investments[t1,] <- investments[t1 - 1,] * as.numeric(1 + rets[current_date,])
  weights[t1,] <- investments[t1,] / sum(investments[t1,])
  portfolio_data[t1, "return"] <- (sum(investments[t1,]) / sum(investments[t1 - 1,])) - 1
  
  # Get current covariance matrix and mean return
  cov_t <- covariance_t(rets, current_date)
  mean_ret <- mean(portfolio_data$return[2:t1])
  
  # Add Information to Portfolio Data
  portfolio_data$var[t1] <- variance(weights[t1,], cov_t)
  portfolio_data$eu[t1] <- expected_utility(mean_ret, portfolio_data$var[t1])
  
  # Get Current Optimal Portfolio Information
  opt_w <- get_opt_weights(cov_t)
  opt_var <- variance(opt_w, cov_t)
  opt_eu <- expected_utility(mu = mean_ret, sigma = opt_var)
  
  # Add Information to Portfolio Data
  portfolio_data$opt_var[t1] <- opt_var
  portfolio_data$opt_eu[t1] <- opt_eu
  
  # Add information to state space
  s_portfolio_data$return <- as.numeric(portfolio_data[t1, "return"])
  s_investments <- s_weights * sum(investments[t1,])

  
  for(s in 1:nrow(s_portfolio_data)){
    # Calculate variance and E[U] for state space 's'
    s_portfolio_data$var[s] <- variance(weights = s_weights[s,], cov = cov_t)
    s_portfolio_data$eu[s] <- expected_utility(mu = mean_ret, sigma = s_portfolio_data$var[s])
    
    # Calculate costs to switch to state space 's'
    s_portfolio_data$tc[s] <- TC(w_opt = weights[t1,], w = s_weights[s,], tc = costs)
    s_portfolio_data$cec[s] <- CEC(opt_EU = opt_eu, 
                                   EU = s_portfolio_data$eu[s], 
                                   capital = sum(investments[t1,]))
  }
  
  # Calculate total costs of switching to each state
  s_portfolio_data$costs <- rowSums(s_portfolio_data[, c("tc", "cec")])
  
  # Identify the minimum cost index and weights
  min_cost_idx <- which.min(s_portfolio_data$costs)
  min_cost_weights[t1,] <- s_weights[min_cost_idx,] %>% as.numeric()
  
  # Add minimum cost rebalancing to portfolio data
  portfolio_data[t1, c("tc", "cec")] <- s_portfolio_data[min_cost_idx, c("tc", "cec")] %>% as.numeric()
  
  if (any(weights[t1,] > w_max) || any(weights[t1,] < w_min)){
    portfolio_data[t1, "rebalance"] <- 1
    weights[t1,] <- min_cost_weights[t1,]
    investments[t1,] <- weights[t1,] * sum(investments[t1,])
    portfolio_data[t1, "return"] <- portfolio_data[t1, "return"] - (portfolio_data[t1, "tc"] / 100)
  }
  
  # Update pb bar
  setTxtProgressBar(pb, t1)
}






