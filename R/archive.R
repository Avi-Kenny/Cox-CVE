
# Simulation to debug Cox model code
if (F) {
  
  if (cfg$sim_which=="Cox") {
    
    #' Run a single simulation (Cox model variance)
    #'
    #' @return A list
    
    one_simulation <- function() {
      
      if (F) {
        dat_orig <- generate_data(n=1000, -2, "Unif(0,1)", "none", surv_true="Cox PH", list(lmbd=1e-3,v=1.5,lmbd2=5e-5,v2=1.5), "two-phase (50%)", "decr", "estimated")
      } # DEBUG
      
      # Generate dataset
      dat_orig <- generate_data(
        L$n, L$alpha_3, L$distr_S, L$edge, surv_true="Cox PH",
        L$sc_params, L$sampling, L$dir, L$wts_type
      )
      
      # Round data values and construct dat
      dat_orig$s <- round(dat_orig$s,2)
      dat_orig$weights <- round(dat_orig$weights,3)
      dat_orig$y <- round(dat_orig$y,0)
      dat <- ss(dat_orig, which(dat_orig$z==1))
      
      # Calculate variance estimates
      s <- 0.5
      res_cox <- cox_var(dat_orig=dat_orig, dat=dat, t=L$t_0, points=s,
                         se_beta=T, se_bshz=T, se_surv=T, se_marg=T)
      
      res_cox <- cox_var(dat_orig=dat_orig, dat=dat, t=L$t_0, points=0.5, se_marg=T, verbose=T)
      res_cox <- cox_var(dat_orig=dat_orig, dat=dat, t=L$t_0, points=c(0.2,0.5,0.8), se_marg=T, verbose=T)
      res_cox <- cox_var(dat_orig=dat_orig, dat=dat, t=L$t_0, points=seq(0.1,0.9,0.1), se_marg=T, verbose=T)
      
      if (F) {
        z_0 <- c(0.3,1,0.5) # Needs to be consistent with the value in cox_var()
        H_0_true <- function(t) {
          L$sc_params$lmbd*exp(-1.7) * t^L$sc_params$v
        }
        true_lp <- sum(c(C$alpha_1,C$alpha_2,L$alpha_3)*z_0)
        true_surv <- exp(-1*exp(true_lp)*H_0_true(L$t_0))
        true_marg <- 1-attr(dat_orig, "r_M0")[26] # Corresponds to A=0.5
      } # DEBUG: intermediate objects
      
      # Construct simulation results object
      # This needs to line up with res_cox based on the se_* flags
      sim_res <- list(
        true_x1 = C$alpha_1,
        est_x1 = res_cox$beta_n[1],
        se_x1 = sqrt(res_cox$var_est_beta[1]),
        true_x2 = C$alpha_2,
        est_x2 = res_cox$beta_n[2],
        se_x2 = sqrt(res_cox$var_est_beta[2]),
        true_s = L$alpha_3,
        est_s = res_cox$beta_n[3],
        se_s = sqrt(res_cox$var_est_beta[3]),
        true_bshz = H_0_true(L$t_0),
        est_bshz = res_cox$est_bshz,
        se_est_bshz = sqrt(res_cox$var_est_bshz),
        true_surv = true_surv,
        est_surv = res_cox$est_surv,
        se_est_surv = sqrt(res_cox$var_est_surv),
        true_marg = true_marg,
        est_marg = res_cox$est_marg,
        se_est_marg = sqrt(res_cox$var_est_marg)
      )
      
      if (F) {
        
        # # Fix a covariate vector and time of interest
        # z_0 <- c(0.3,1,0.5) # c(W1,W2,A)
        # 
        # # Calculate the cumulative hazard via predict()
        # newdata <- data.frame(y=L$t_0, delta=1, x1=z_0[1],
        #                       x2=z_0[2], s=z_0[3])
        # pred <- predict(res_cox$model, newdata=newdata, type="expected", se.fit=T)
        
        
        # Add additional results
        # sim_res$true_cmhz = exp(true_lp) * H_0_true(L$t_0) # Should roughly equal pred$se.fit
        # sim_res$est_cmhz = exp(sum(res_cox$beta_n*z_0))*est_bshz # Should equal pred$fit
        # sim_res$est_surv = exp(-1*exp(sum(res_cox$beta_n*z_0))*est_bshz)
        # sim_res$se_cmhz_MC = sqrt(res_cox$var_cmhz_est)
        # sim_res$se_surv_MC = sqrt(res_cox$var_surv_est)
        # sim_res$se_bshz_Cox = pred$se.fit
        
        # Debugging the Breslow estimator
        # sim_res$se_bshz_MC = sqrt(res_cox$var_bshz_est)
        # sim_res$se_bshz_MC2 = sqrt(res_cox$var_bshz_est2) # New derivation
        
        # sim_res$se_x1_Cox = as.numeric(sqrt(diag(vcov(res_cox$model)))[1])
        # sim_res$se_x1_alt = as.numeric(sqrt(diag(res_cox$I_tilde_inv)/L$n)[1])
        
      } # DEBUG
      
      return(sim_res)
      
    }
    
  }
  
}
