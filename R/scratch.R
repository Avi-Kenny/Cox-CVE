# Bootstrap testing
if (F) {
  
  for (i in c(1:5)) {
    
    L <- list(
      n = 1000,
      t_0 = 200,
      alpha_3 = -2,
      dir = "decr",
      sc_params = list(lmbd=1.16e-6, v=1.5, lmbd2=5e-5, v2=1.5),
      distr_S = "Unif(0,1)",
      edge = "none",
      surv_true = "Cox PH",
      sampling = "two-phase (50%)",
      wts_type = "estimated",
      estimator = list(spline_df=1, edge_ind=F),
      bootstrap = c(F,T)
    )
    
    dat_orig <- generate_data(L$n, L$alpha_3, L$distr_S, L$edge, L$surv_true,
                              L$sc_params, L$sampling, L$dir, L$wts_type)
    dat_orig$delta2 <- dat_orig$delta * In(dat_orig$y<=L$t_0)
    
    print(paste0("Size of phase-two sample: ", sum(dat_orig$z)))
    print(paste0("Number of events: ", sum(dat_orig$delta)))
    print(paste0("Number of events (less than t_0): ", sum(dat_orig$delta2)))
    
  }
  
}

# Confirm equivalence of old and new transformations
if (F) {
  
  old_lims <- function(r, n, z, sigma) {
    expit(logit(r)+c(-1,1)*n^(-1/2)*z*deriv_logit(r)*sigma)
  }
  
  new_lims <- function(r, n, z, sigma) {
    (1 + (1/r-1)*exp((c(1,-1)*n^(-1/2)*z*sigma)/(r*(1-r))))^-1
  }
  
  both <- function(s, n, z, sigma) {
    print(old_lims(s, n, z, sigma))
    print(new_lims(s, n, z, sigma))
    invisible()
  }
  
  both(s=0.5, n=100, z=1.96, sigma=2)
  both(s=0.5, n=100, z=1.96, sigma=4)
  both(s=0.5, n=500, z=1.96, sigma=2)
  both(s=0.5, n=500, z=1.96, sigma=4)
  both(s=0.1, n=100, z=1.96, sigma=2)
  both(s=0.1, n=100, z=1.96, sigma=4)
  both(s=0.1, n=500, z=1.96, sigma=2)
  both(s=0.1, n=500, z=1.96, sigma=4)
  
}
