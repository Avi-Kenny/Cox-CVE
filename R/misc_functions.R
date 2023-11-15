#' Logit function
#' 
#' @param u Numeric input
#' @return Numeric output
logit <- function(u) { log(u/(1-u)) }



#' Expit function
#' 
#' @param u Numeric input
#' @return Numeric output
expit <- function(u) { 1 / (1+exp(-u)) }



#' Derivative of expit function
#' 
#' @param u Numeric input
#' @return Numeric output
deriv_expit <- function(u) { exp(u) / ((1+exp(u))^2) }



#' Alias for indicator (as.integer) function
#' 
#' @param u Logical input
#' @return BInary (integer) output
In <- as.integer



#' Derivative of logit function
#' 
#' @param u Numeric input
#' @return Numeric output
deriv_logit <- function(u) { 1 / (u-u^2) }



#' Helper function for debugging; prints timestamps
#' 
#' @param num Number
#' @param msg Message
chk <- function(num, msg="") {
  if (msg=="") {
    str <- paste0("Check ", num, ": ", Sys.time())
  } else {
    str <- paste0("Check ", num, " (", msg, "): ", Sys.time())
  }
  print(str)
}



#' Probability of being sampled into subcohort
#' 
#' @param sampling One of c("iid", "two-phase (6%)", "two-phase (72%)",
#'     "two-phase (70% random)", "two-phase (6% random)")
#' @param delta Component of dataset returned by generate_data()
#' @param y Component of dataset returned by generate_data()
#' @param x Component of dataset returned by generate_data()
#' @return A vector of probabilities of sampling
#' @notes
#'   - Only used for simulation; for the real analysis, the weights are
#'     calculated separately
Pi <- function(sampling, delta, y, x) {
  
  if (sampling=="iid") {
    probs <- rep(1, length(delta))
  } else if (sampling=="two-phase (25% random)") {
    probs <- rep(0.25, length(delta))
  } else {
    ev <- In(delta==1 & y<=C$t_0) # !!!!!
    if (sampling=="two-phase (6%)") {
      probs <- ev + (1-ev)*expit(x$x1+x$x2-3.85)
    } else if (sampling=="two-phase (72%)") {
      probs <- ev + (1-ev)*expit(x$x1+x$x2-0.1)
    } else if (sampling=="two-phase (50%)") {
      probs <- ev + (1-ev)*expit(x$x1+x$x2-1)
    } else if (sampling=="two-phase (25%)") {
      probs <- ev + (1-ev)*expit(x$x1+x$x2-2.2)
    } else if (sampling=="x1") {
      probs <- (0.2 + 0.6*x$x1)
    } else if (sampling=="x2") {
      probs <- (0.2 + 0.6*x$x2)
    }
    
  }
  
  return(probs)
  
}



#' Return IP weights
#' 
#' @param dat_orig Dataset returned by generate_data()
#' @param scale One of c("none", "stabilized")
#' @param type One of c("true", "estimated")
#' @param return_strata Whether the discrete two-phase sampling strata
#'     membership variable should be returned
#' @return A sum-to-one vector of weights
#' @notes
#'   - Only used for simulation; for the real analysis, the weights are
#'     calculated separately
wts <- function(dat_orig, scale="stabilized", type="true", return_strata=F) {
  
  sampling <- attr(dat_orig,"sampling")
  Pi_0 <- Pi(sampling, dat_orig$delta, dat_orig$y,
             subset(dat_orig, select=c("x1","x2")))
  strata1 <- In(factor(Pi_0))
  
  if (type=="true") {
    
    weights <- dat_orig$z / Pi_0
    
  } else if (type=="estimated") {
    
    Pi_vals <- c()
    for (i in c(1:max(strata1))) {
      Pi_vals[i] <- sum(In(strata1==i)*dat_orig$z) / sum(In(strata1==i))
      if (Pi_vals[i]==0) {
        # Hack to avoid NA values in small sample sizes
        warning(paste0("stratum ", i, " had no one sampled."))
        Pi_vals[i] <- 1
      }
    }
    weights <- dat_orig$z / Pi_vals[strata1]
    
  }
  
  if (scale=="stabilized") {
    sm <- sum(weights) / length(dat_orig$z)
    weights <- weights / sm
  }
  
  if (!return_strata) {
    return(weights)
  } else {
    return(list(weights=weights, strata=strata1))
  }
  
}



