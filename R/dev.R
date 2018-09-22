# Libraries ---------------------------------------------------------------
library(RcppAlgos)
library(magrittr)
library(foreach)
library(doSNOW)
library(quantmod)

# parameters --------------------------------------------------------------

setwd("~/Github/R")

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
w_incr <- 0.01                 # Weight increment size
 
steps <- 100                   # Number of incremental steps in monte-carlo sim
paths <- 2                     # Number of paths to simulate per state

# load pricing data
source("./pricing-data.R")

rets.weekly <- pxs %>% lapply(weeklyReturn) %>% do.call("cbind", .)
pxs.weekly <- pxs[index(rets.weekly)]


# Load Functions ----------------------------------------------------------

source("./functions.R")

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



steps <- 240

k = 0

# Initialized the T+1 expected future cost values
J_t1 <- rep(k, nrow(W))

# Starting at time t = T, walk backwards
for(step in steps:0){
  
  # Set the expected future cost to be Inf at default
  J_t <- rep(Inf, nrow(W))
  
  for(i in 1:nrow(W)){
    
    # For each possible weight now, at time t
    w_t <- W[i,]
    
    # Get the current expected cost of being in this state
    EG <- EG_t(w_t, w_opt, W, ret_start, cov_mat = cov_start)
    J_t[i] <- EG + (g * J_t1[i])
  }
  
  J_t1 <- J_t
  
}


library(plot3D)

state_space <- as.data.frame(W)
cost_vals <- J_t1
optimal_weight <- state_space[which.min(cost_vals),]

scatter3D(state_space$V1, state_space$V2, state_space$V3, colvar = cost_vals,
          alpha = 0.6, theta = 115, phi = 0, clab = "E[Future Cost]", bty = "g",
          ticktype = "detailed", main = "State Space Cost Values")
points3D(optimal_weight[,1], optimal_weight[,2], optimal_weight[,3], type = "h",
          pch = 19, add = T)







