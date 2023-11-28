######################.
##### Estimation #####
######################.

if (cfg$sim_which=="estimation") {
  
  #' Run a single simulation (estimation)
  #'
  #' @return A list with the estimates and CI limits of the causal dose-response
  #'     curve evaluated at the midpoint and the endpoint of the domain
  one_simulation <- function() {
    
    # Generate dataset
    # batch({
    #   dat_orig <- generate_data(L$n, L$alpha_3, L$distr_S, L$edge, L$surv_true,
    #                             L$sc_params, L$sampling, L$dir, L$wts_type)
    # })
    
    # Generate dataset
    dat_orig <- generate_data(L$n, L$alpha_3, L$distr_S, L$edge, L$surv_true,
                              L$sc_params, L$sampling, L$dir, L$wts_type)
    
    # Load data into correct format
    dat <- load_data(
      time="y", event="delta", vacc="a", marker="s", covariates=c("x1","x2"),
      weights="weights", ph2="z", data=dat_orig
    )

    # Obtain estimates
    if (L$bootstrap) { ci_type <- "none" } else { ci_type <- "transformed" }
    ests <- vaccine::est_ce(
      dat = dat,
      type = "Cox",
      t_0 = L$t_0,
      s_out = C$points,
      ci_type = ci_type,
      params_cox = params_ce_cox(spline_df = L$estimator$spline_df,
                                 edge_ind = L$estimator$edge_ind)
    )
    if (L$bootstrap) {
      b_ests <- bootstrap_ses(dat_orig, n_boot=1000)
      ests$cr$ci_lower <- b_ests$ci_lower
      ests$cr$ci_upper <- b_ests$ci_upper
    }

    # Return results
    r_M0 <- attr(dat_orig, "r_M0")
    res_list <- list()
    for (i in 1:length(C$points)) {
      m <- format(C$points[i], nsmall=2)
      res_list[paste0("r_M0_",m)] <- r_M0[i]
      res_list[paste0("r_Mn_",m)] <- ests$cr$est[i]
      res_list[paste0("ci_lo_",m)] <- ests$cr$ci_lower[i]
      res_list[paste0("ci_up_",m)] <- ests$cr$ci_upper[i]
      if (!is.null(L$return_se)) { res_list[paste0("se_",m)] <- ests$cr$se[i] }
    }
    
    if (!is.null(L$return_num_events) && L$return_num_events) {
      res_list$num_events <- sum(dat$delta)
      res_list$num_events2 <- sum(dat$delta*In(dat$y<=L$t_0))
      if (L$bootstrap) {
        res_list$num_succ <- b_ests$num_succ
        res_list$num_errs <- b_ests$num_errs
      } else {
        res_list$num_succ <- NA
        res_list$num_errs <- NA
      }
    }
    
    return(res_list)
    
  }
  
}
