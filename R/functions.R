# Functions ---------------------------------------------------------------

# Utility Function
U <- function(mu, sigma, type = c("quad", "log", "power")){
  switch (type,
          "quad" = mu - ((alpha / 2) * sigma^2),
          "log" = log10(1 + mu) - ((sigma^2) / (2 * (1 + mu)^2 )),
          "power" = 1 - (1 / ( 1 + mu)) + ((sigma^2) / (1 + mu)^3),
  )
}

# Transaction costs
tau_t <- function(w_t, w_t1){
  trans_costs * abs(w_t1 - w_t)
}

# Certainty Equivalent Return
r_CE <- function(w, mean_rets, cov_mat){
  U(w %*% mean_rets, w %*% cov_mat %*% w, type = "log")
}

# Certainty Equivalent Costs
e_t <- function(w_t, w_opt, mean_rets, cov_mat){
  r_CE(w_opt, mean_rets, cov_mat) - r_CE(w_t, mean_rets, cov_mat)
}

# Random Noise Process - Normal Distribution
nu <- function(n, mu, sigma){
  round(rnorm(n, mu, sigma)) * w_incr # Generate N random numbers
}

# State Transition Function - Additive Dynamic Model
h <- function(w_t, u_t, nu_t){
  (w_t + u_t) + nu_t
}

# State space generator
get_state_space <- function(n, w_min, w_max, w_incr){
  require(RcppAlgos)
  vec <- seq(w_min, w_max, w_incr)
  RcppAlgos::permuteGeneral(vec, n, repetition = TRUE, constraintFun = "sum", comparisonFun = "==", limitConstraints = 1)
}

# Identifies the probability of return X occuring
Prob <- function(x){
  dnorm(x*10, mu, sigma)
}


# Simulate weight movement n-steps into the future
simulate <- function(n, w_0){
  dw <- rbind(w_0,
              matrix(nu(N * n, mu, sigma), ncol = N)
  )
  rownames(dw) <- 0:n
  sapply(seq_along(1:N), 
         function(j) cumsum(dw[,j])
  )
}


# Current Time cost
G <- function(w_t1, w_t, mean_rets, cov_mat, w_opt = w_opt){
  tc <- sum(tau_t(w_t = w_t, w_t1 = w_t1))
  sub_optc <- e_t(w_t = w_t1, w_opt = w_opt, mean_rets = mean_rets, cov_mat = cov_mat)
  
  return(tc + sub_optc)
}


J <- function(mat,  mean_rets, cov_mat, w_opt = w_opt){
  
}

# Check if an allocation is in W
is.in_state_space <- function(w_0, W){
  M <- W
  for(i in 1:N){
    M <- matrix(M[M[,i] == w_0[i],], ncol = N)
  }
  
  if(nrow(M) > 0){
    return(T)
  } else {
    return(F)
  }
} 

#' Expected Current Cost Function
#' 
#' Calculates the Current Expected Cost of having 
#'
#' @param w_t 
#' @param w_opt 
#' @param W 
#' @param mean_rets 
#' @param cov_mat 
#'
#' @return
#' @export
#'
#' @examples
EG_t <- function(w_t, w_opt, W, mean_rets, cov_mat){
  dw <- W - w_t
  
  p <- apply(Prob(dw), 1, prod)
  
  p <- p / sum(p)
  
  costs <- apply(W, 1, G, mean_rets = mean_rets, cov_mat = cov_mat, 
                 w_t = w_t,     # current weight
                 w_opt = w_opt  # optimal weight
  )
  
  p %*% costs
}

J <- function(w_mat, w_opt, W, mean_rets, cov_mat, g, type = c("iter", "recur")){
  
  helper_recur <- function(w_mat, k = 0){
    
    if(is.null(dim(w_mat))){
      
      return(k)
      
    } else {
      EG <- EG_t(w_t = w_mat[1,], w_opt = w_opt, W = W, mean_rets = mean_rets, cov_mat = cov_mat)
      
      return(EG + (g * helper_recur(w_mat[-1,])))
    }
  }
  
  helper_iter <- function(w_mat, k = 0){
    
    accum <- 0
    
    for (t_ in seq_along(1:nrow(w_mat))) {
      EG <- EG_t(w_t = w_mat[t_,], w_opt = w_opt, W = W, mean_rets = mean_rets, cov_mat = cov_mat)
      
      if(t_ == nrow(w_mat)) EG <- EG + k
      
      accum <- accum + (g^(t_) * EG)
    } 
    
    return(accum)
  }
  
  if (length(type) != 1 && !(type %in% c("iter", "recur"))) stop("Parameter 'type' must be set to 'iter' or 'recur'")
  
  if(type == "iter"){
    return(helper_iter(w_mat))
  }
  
  if(type == "recur"){
    return(helper_recur(w_mat))
  }
  
}






























