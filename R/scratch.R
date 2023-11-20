# TEMP bootstrap viz
if (F) {
  
  ppp <- 0.5
  # sim=readRDS("sim.rds")
  summ_cov <- list()
  for (i in c(1:51)) {
    m <- format(round(i/50-0.02,2), nsmall=2)
    summ_cov[[i]] <- list(
      stat = "coverage",
      name = paste0("cov_",m),
      truth = paste0("r_M0_",m),
      lower = paste0("ci_lo_",m),
      upper = paste0("ci_up_",m),
      na.rm = T # !!!!!
    )
  }
  summ_other <- list(
    list(stat="mean", x="runtime", name="runtime"),
    list(stat="mean", x="num_events", name="num_events"),
    list(stat="mean", x="num_succ", name="num_succ"),
    list(stat="mean", x="num_errs", name="num_errs")
  )
  
  summ <- do.call(SimEngine::summarize, c(list(sim),c(summ_cov, summ_other)))
  
  p_data <- pivot_longer(
    data = summ,
    cols = -c("level_id", "n_reps", names(sim$levels), "runtime", "num_events",
              "num_succ", "num_errs"),
    names_to = c("stat","point"),
    names_sep = "_"
  )
  p_data %<>% mutate(point = as.numeric(point))
  
  # Set faceting vectors
  distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")
  
  p_data %<>% dplyr::filter(point==ppp)
  p_data %<>% dplyr::mutate(
    bootstrap = ifelse(bootstrap, "Bootstrap", "Analytic")
  )
  
  # Coverage plot
  plot <- ggplot(
    p_data,
    aes(x=t_0, y=value, color=bootstrap,
        group=bootstrap)) +
    geom_hline(aes(yintercept=0.95), linetype="longdash", color="grey") +
    geom_line() +
    facet_wrap(~sc_params) +
    scale_y_continuous(labels=percent) +
    theme(legend.position="bottom") +
    labs(y="Coverage (%)", x="t_0", color=NULL, title=paste("Point:", ppp))
  print(plot)

}

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
