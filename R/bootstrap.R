#' Generate bootstrap SE estimates
#' 
#' @param dat_orig A data object returned by generate_data
#' @param n_boot Integer; number of bootstrap replicates
#' @return A list with keys ci_lower and ci_upper; the corresponding values are
#'     each a vector of the same length as s_out representing the pointwise
#'     confidence bands
bootstrap_ses <- function(dat_orig, n_boot) {
  
  # Objects to hold replicate info
  ests_boot <- matrix(nrow=n_boot, ncol=length(C$points))
  num_succ <- 0
  num_errs <- 0
  
  for (i in c(1:n_boot)) {
    
    # Create resampled data object
    {
      dat_orig_resampled <- dat_orig[F,]
      tps_strata <- attr(dat_orig, "tps_strata")
      tps_strata_unique <- sort(unique(tps_strata))
      subcohort <- attr(dat_orig, "subcohort")
      
      # Resample IDs
      ids_new <- c()
      for (str in tps_strata_unique) {
        ids_subcohort <- which(tps_strata==str & subcohort)
        ids_nonsubcohort <- which(tps_strata==str & !subcohort)
        ids_new <- c(ids_new,
                     sample(ids_subcohort, replace=T),
                     sample(ids_nonsubcohort, replace=T))
      }
      if (!length(ids_new)==nrow(dat_orig)) { stop("Error generating ids_new") }

      for (j in c(1:nrow(dat_orig))) {
        dat_orig_resampled[j,] <- dat_orig[ids_new[j],]
      }
      
      # Recompute weights
      w_strata_unique <- sort(unique(dat_orig_resampled$strata))
      w_strata_probs <- sapply(w_strata_unique, function(str) {
        num_in_stratum <- sum(dat_orig_resampled$strata==str)
        num_selected <- sum(dat_orig_resampled$strata==str &
                              dat_orig_resampled$z)
        if (num_selected==0) { warning("Stratum with no one selected.") }
        return(num_selected/num_in_stratum)
      })
      dat_orig_resampled$weights <- dat_orig_resampled$z /
        w_strata_probs[dat_orig_resampled$strata]
      
    }
    
    dat <- load_data(
      time="y", event="delta", vacc="a", marker="s", covariates=c("x1","x2"),
      weights="weights", ph2="z", data=dat_orig_resampled
    )
    
    tryCatch(
      expr = {
        ests <- vaccine::est_ce(
          dat = dat,
          type = "Cox",
          t_0 = L$t_0,
          s_out = C$points,
          ci_type = "none",
          params_cox = params_ce_cox(spline_df = L$estimator$spline_df,
                                     edge_ind = L$estimator$edge_ind)
        )
        ests_boot[i,] <- ests$cr$est
        num_succ <- num_succ + 1
      },
      error = function(e) {
        num_errs <<- num_errs + 1
      }
    )
    
  }
  
  ci_lower <- ci_upper <- rep(NA, length(C$points))
  for (i in c(1:length(C$points))) {
    lims <- as.numeric(quantile(ests_boot[,i], probs=c(0.025,0.975), na.rm=T))
    ci_lower[i] <- lims[1]
    ci_upper[i] <- lims[2]
  }
  
  if (!(num_succ+num_errs==n_boot)) {
    stop(paste0("num_succ: ", num_succ, ", num_errs: ", num_errs, ", n_boot: ",
                n_boot, "."))
  }
  
  return(list(
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    num_succ = num_succ,
    num_errs = num_errs
  ))
  
}
