# Set global constants
C <- list(
  points = round(seq(0,1,0.02),2),
  alpha_1 = 0.5,
  alpha_2 = 0.7
)

# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Estimation: no edge mass
  # Figures: sample_paths, sim_est_bias, sim_est_cov, sim_est_se
  level_sets[["estimation_1"]] <- list(
    n = 1000,
    t_0 = 200,
    alpha_3 = -2,
    dir = "decr",
    sc_params = list("sc_params"=list(lmbd=2e-4, v=1.5, lmbd2=5e-5, v2=1.5)),
    distr_S = c("Unif(0,1)", "N(0.5,0.04)"),
    edge = "none",
    surv_true = c("Cox PH", "S-shaped", "Cubic"),
    sampling = "two-phase (50%)",
    wts_type = "estimated",
    estimator = list(
      "Cox (basic)" = list(spline_df=1, edge_ind=F),
      "Cox (spline 4 df)" = list(spline_df=4, edge_ind=F),
      "Cox (spline 8 df)" = list(spline_df=8, edge_ind=F)
    ),
    bootstrap = F
  )
  
  # Estimation: edge mass
  # Figures: sim_est_edge_bias, sim_est_edge_cov, sim_est_edge_se
  level_sets[["estimation_2"]] <- list(
    n = 1000,
    t_0 = 200,
    alpha_3 = -2,
    dir = "decr",
    sc_params = list("sc_params"=list(lmbd=2e-4, v=1.5, lmbd2=5e-5, v2=1.5)),
    distr_S = "N(0.5,0.04)",
    edge = "expit 0.1",
    surv_true = c("Cox PH", "Step"),
    sampling = "two-phase (50%)",
    wts_type = "estimated",
    estimator = list(
      "Cox (basic)" = list(spline_df=1, edge_ind=F),
      "Cox (edge)" = list(spline_df=1, edge_ind=T),
      "Cox (spline 4 df)" = list(spline_df=4, edge_ind=F),
      "Cox (edge + spline 4 df)" = list(spline_df=4, edge_ind=T)
    ),
    bootstrap = F
  )
  
  # Estimation: standard error estimation
  # Figures: se_est
  level_sets[["estimation_3"]] <- list(
    n = c(100, 200, 400, 800, 1600, 3200),
    t_0 = 200,
    alpha_3 = -2,
    dir = "decr",
    sc_params = list("sc_params"=list(lmbd=2e-4, v=1.5, lmbd2=5e-5, v2=1.5)),
    distr_S = c("Unif(0,1)", "N(0.5,0.04)"),
    edge = "none",
    surv_true = c("Cox PH", "S-shaped", "Cubic"),
    sampling = "two-phase (50%)",
    wts_type = "estimated",
    return_se = T,
    estimator = list(
      "Cox (basic)" = list(spline_df=1, edge_ind=F)
    ),
    bootstrap = F
  )
  
  # Estimation: bootstrap vs. analytic SEs
  # Figures: bootstrap_vs_analytic
  level_sets[["estimation_4"]] <- list(
    n = 1000,
    t_0 = c(200,500,1000),
    alpha_3 = -2,
    dir = "decr",
    # sc_params = list("sc_params"=list(lmbd=2e-4, v=1.5, lmbd2=5e-5, v2=1.5)),
    # sc_params = list("sc_params"=list(lmbd=1e-5, v=1.5, lmbd2=5e-5, v2=1.5)),
    # sc_params = list("sc_params"=list(lmbd=5e-6, v=1.5, lmbd2=5e-5, v2=1.5)),
    sc_params = list(
      "lmbd_23" = list(lmbd=1.16e-6, v=1.5, lmbd2=5e-5, v2=1.5),
      # "lmbd_24" = list(lmbd=2.32e-6, v=1.5, lmbd2=5e-5, v2=1.5),
      # "lmbd_25" = list(lmbd=4.64e-6, v=1.5, lmbd2=5e-5, v2=1.5),
      # "lmbd_26" = list(lmbd=9.28e-6, v=1.5, lmbd2=5e-5, v2=1.5),
      "lmbd_27" = list(lmbd=1.86e-5, v=1.5, lmbd2=5e-5, v2=1.5),
      
      "lmbd_28" = list(lmbd=1.16e-6, v=1.6, lmbd2=5e-5, v2=1.5),
      "lmbd_29" = list(lmbd=1.86e-5, v=1.6, lmbd2=5e-5, v2=1.5),
      "lmbd_30" = list(lmbd=1.16e-6, v=1.4, lmbd2=5e-5, v2=1.5),
      "lmbd_31" = list(lmbd=1.86e-5, v=1.4, lmbd2=5e-5, v2=1.5)
      
    ),
    # distr_S = c("Unif(0,1)", "N(0.5,0.04)"),
    distr_S = "Unif(0,1)", # !!!!!
    edge = "none",
    surv_true = "Cox PH",
    # surv_true = c("Cox PH", "S-shaped", "Cubic"),
    sampling = "two-phase (50%)",
    wts_type = "estimated",
    estimator = list("Cox (basic)" = list(spline_df=1, edge_ind=F)),
    bootstrap = c(F,T),
    # bootstrap = F, # !!!!!
    return_num_events = T
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
  # if (cfg$sim_level_set=="asdf") { cfg$keep = c(1:3,7:9,16:18,22:24) }
  
}
