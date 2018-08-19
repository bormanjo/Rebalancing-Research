# Libraries ---------------------------------------------------------------
library(RcppAlgos)
library(magrittr)
library(foreach)
library(doSNOW)

# parameters --------------------------------------------------------------

# Asset to Invest in
tickers <- c("XLK", "XLF", "XLE")

N <- length(tickers)               # Number of Assets

g <- 0.9                           # Gamma, discount factor
mu <- 0                            # Mu, Random Variable Mean
sigma <- 1.8                       # Sigma, Random Variable Std. Dev.
trans_costs <- c(40, 50, 60)/1000  # Transaction Costs

alpha <- 0.5                   # Alpha, Utility function coefficient

w_min <- 0                     # Minimum asset weight
w_max <- 0.5                   # Maximum asset weight
w_incr <- 0.0025               # Weight increment size

steps <- 100                   # Number of incremental steps in monte-carlo sim

# load pricing data
source("./R/pricing-data.R")

# Load Functions ----------------------------------------------------------

source("./R/functions.R")

# Parallel Backend --------------------------------------------------------

cl <-  makeCluster(parallel::detectCores())
doSNOW::registerDoSNOW(cl)
on.exit(parallel::stopCluster(cl))

# Begin Script ------------------------------------------------------------

W <- get_state_space(N, w_min, w_max, w_incr)

w_opt <- W[which.min(rowSums(W^2)),]
w_opt <- (solve(cov_start) * ret_start) / sqrt(t(ret_start) %*% solve(cov_start) %*% ret_start)
  
w_0 <- w_opt

seq(-1,1,w_incr) %>% plot(y = Prob(.), type = "b", main = "Probability of % Change Occurring")

V <- rep(Inf, nrow(W))

idx <- 1:length(V)

pb <- txtProgressBar(max = length(V), style = 3)
progress <- function(x) setTxtProgressBar(pb, x)
opts <- list(progress = progress)

V[idx] <- foreach(i = idx, .combine = c, .options.snow = opts) %dopar% {
  J(w_mat = simulate(40, W[1,]), 
    w_opt = w_opt,
    W = W,
    mean_rets = ret_start,
    cov_mat = cov_start,
    g = g, type = "recur")
}

close(pb)