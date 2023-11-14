# Set global constants
C <- list(
  points = round(seq(0,1,0.02),2),
  alpha_1 = 0.5,
  alpha_2 = 0.7,
  t_0 = 200,
  appx = list(t_0=1, x_tol=25, s=0.01)
)

# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Estimation: no edge mass
  # Figures: sample_paths, sim_est_bias, sim_est_cov, sim_est_se
  level_sets[["estimation_1"]] <- list(
    n = 1000,
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
    )
  )
  
  # Estimation: edge mass
  # Figures: sim_est_edge_bias, sim_est_edge_cov, sim_est_edge_se
  level_sets[["estimation_2"]] <- list(
    n = 1000,
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
    )
  )
  
  # Estimation: standard error estimation
  # Figures: se_est
  level_sets[["estimation_3"]] <- list(
    n = c(100, 200, 400, 800, 1600, 3200),
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
    )
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
  # if (cfg$sim_level_set=="asdf") { cfg$keep = c(1:3,7:9,16:18,22:24) }
  
}
