library(xts)
library(quantmod)
library(magrittr)
library(dygraphs)
library(lubridate)
library(quadprog)



# Functions ---------------------------------------------------------------

variance <- function(weights, cov){
  weights <- as.numeric(weights)
  t(weights) %*% cov %*% weights
}

expected_utility <- function(mu, sigma){
  log10(1 + mu) - ( sigma / (2 * (1 + mu)^2))
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
  sum(tc * (w_opt - w))
}

# Parameters --------------------------------------------------------------------

tickers <- c("MSFT", "AAPL", "GOOG")
start_years <- 3
costs <- c(40, 50, 60)
capital <- 1e7

w_min <- 0.10
w_max <- 0.40
w_incr <- 0.10

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

cov_start <- cov(rets[paste0("/",date_start)])

temp <- rets[paste0(date_start,"/")]

template <- xts(x = matrix(data = 0, nrow = nrow(temp), ncol = 3), order.by = index(temp))
rm(temp)
colnames(template) <- tickers

# Optimal Portfolio -------------------------------------------------------


# Minimum Variance
w_opt <- solve(cov_start) %*% rep(1, length(tickers))
w_opt <- w_opt[,1] / sum(w_opt)


# No Rebalancing ----------------------------------------------------------

NR_investment <- template
NR_weights <- template
NR_portfolio <- xts(matrix(0, nrow = nrow(template), ncol = 7), order.by = index(template))
colnames(NR_portfolio) <- c("Total Return", "Variance", "ExpUtil", "Opt Variance", "Opt ExpUtil", "TC", "CEC")

NR_date_start <- index(NR_investment) %>% first()
NR_date_end <- index(NR_investment) %>% last()

NR_weights[NR_date_start,] <- w_opt
NR_investment[NR_date_start,] <- w_opt * capital

for( t1 in 2:nrow(NR_investment) ){

  NR_investment[t1,] <- NR_investment[t1-1,] * coredata(rets[t1,] + 1)
  NR_weights[t1,] <- coredata(NR_investment[t1,]) / sum(NR_investment[t1,])
  
  NR_portfolio[t1, "Total Return"] <- ( sum(NR_investment[t1,]) / sum(NR_investment[t1 - 1,]) ) - 1
  
  NR_portfolio[t1, "Variance"] <- variance(weights = NR_weights[t1,], cov = cov_start)
  
  NR_portfolio[t1, "ExpUtil"] <- expected_utility(mu = NR_portfolio[2:t1,] %>% mean, sigma = NR_portfolio[t1, "Variance"])
  
  NR_portfolio[t1, "Opt Variance"] <- variance(weights = w_opt, cov = cov_start)
  
  NR_portfolio[t1, "Opt ExpUtil"] <- expected_utility(mu = NR_portfolio[2:t1,] %>% mean, sigma = NR_portfolio[t1, "Variance"])
}

(NR_portfolio$`Total Return` + 1) %>% cumprod %>% plot(main = "Cumulative Return")
NR_weights %>% plot(main = "Weights")
NR_investment %>% plot(main = "Asset Investment Amount")


# Dynamic Rebalancing -----------------------------------------------------

DR_investment <- NR_investment
DR_mc_weights <- template

DR_high_bound <- template
DR_low_bound <- template

# Contains the portfolio rebalancing data
DR_portfolio <- xts(matrix(0, nrow = nrow(template), ncol = 4), order.by = index(template))
colnames(DR_portfolio) <- c("TC", "CEC", "Costs", "Rebal")

# Contains the hypothetical data with which we use to simulate on
DR_states <- xts(matrix(0, nrow = nrow(template), ncol = 8), order.by = index(template))

state_space <- get_state_space(n = length(tickers), w_min, w_max, w_incr)

DR_state_weights <- state_space
colnames(DR_state_weights) <- tickers

DR_states <- matrix(0, nrow = nrow(state_space), ncol = 8)
colnames(DR_states) <- c("Total Return", "Variance", "ExpUtil", "Opt Variance", "Opt ExpUtil", "TC", "CEC", "Costs")

DR_date_start <- index(DR_investment) %>% first()
DR_date_end <- index(DR_investment) %>% last()

DR_mc_weights[DR_date_start,] <- w_opt

for( t1 in 1:nrow(DR_investment)){
  DR_portfolio[t1, "Costs"] <- 0
  DR_mc_weights[t1, ] <- rep(0, length(tickers))
  
  DR_state_investment <- DR_state_weights * capital
  DR_states[, "Total Return"] <- NR_portfolio[t1, "Total Return"]
  
  for (t2 in 1:nrow(DR_state_weights)) {
    DR_states[t2, "Variance"] <- variance(DR_state_weights[t2, ], cov_start)
    DR_states[t2, "ExpUtil"] <- expected_utility(NR_portfolio[1:t1, "Total Return"] %>% mean, DR_states[t2, "Variance"])
    
    DR_states[t2, "Opt Variance"] <- variance(w_opt, cov_start)
    DR_states[t2, "Opt ExpUtil"] <- expected_utility(NR_portfolio[1:t1, "Total Return"] %>% mean, DR_states[t2, "Opt Variance"])
    
    DR_states[t2, "CEC"] <- CEC(DR_states[t2, "Opt ExpUtil"], DR_states[t2, "ExpUtil"], capital = capital)
    DR_states[t2, "TC"] <- TC(w_opt = w_opt, w = DR_state_weights[t2, ], tc = costs)
    DR_states[t2, "Costs"] <- DR_states[t2, c("CEC", "TC")] %>% sum
  }
  
  # Get the index of the state with the minimum weight
  min_cost_idx <- DR_states[, "Costs"] %>% which.min()
  
  if( any(DR_states[, "Costs"] < 0)){
    DR_high_bound[t1, ] <- DR_state_weights[ min_cost_idx, ]
  }
  
  if( any(DR_states[, "Costs"] > 0)){
    DR_low_bound[t1, ] <- DR_state_weights[ DR_states[, "Costs"] %>% which.max(), ]
  }
  
  DR_mc_weights[t1, ] <- DR_state_weights[min_cost_idx, ]
  DR_portfolio[t1, c("CEC", "TC")] <- DR_states[min_cost_idx, c("CEC", "TC")] %>% as.numeric()
}

DR_weights <- NR_weights
DR_rets <- rets[paste0(DR_date_start, "/", DR_date_end)]

DR_portfolio[DR_date_end, "Costs"] <- DR_portfolio[DR_date_end, c("TC", "CEC")] %>% sum()
for(t1 in (nrow(DR_portfolio)-1):1){
  DR_portfolio[t1, "Costs"] = DR_portfolio[t1, c("TC", "CEC")] %>% sum() + DR_portfolio[t1 + 1, "Costs"]
}

for(t1 in 1:nrow(DR_portfolio)){
  if(all(DR_weights[t1,] > DR_low_bound[t1,]) && all(DR_weights[t1,] < DR_high_bound[t1,])){
    DR_portfolio[t1, "Rebal"] <- 0
  } else {
    DR_portfolio[t1, "Rebal"] <- 1
    DR_weights[t1 + 1,] <- DR_mc_weights[t1,]
    DR_investment[t1 + 1,] <- DR_weights[t1 + 1,] * sum(DR_investment[t1,])
    
    for(t2 in (t1+2):nrow(DR_portfolio)){
      if(t2 > nrow(DR_portfolio)) break()
      DR_investment[t2,] <- DR_investment[t2-1,] * as.numeric(DR_rets[t2,]+1)
      DR_weights[t2,] <- DR_investment[t2,] / sum(DR_investment[t2,])
    }
  }
}









