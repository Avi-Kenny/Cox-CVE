#########################################.
##### VIZ: Estimation (bias,cov,sd) #####
#########################################.

# Figures produced: sim_est_edge_bias, sim_est_edge_cov, sim_est_edge_se,
#                   sim_est_bias, sim_est_cov, sim_est_se

for (edge in c(F,T)) {
  
  if (edge) {
    sim <- readRDS("SimEngine.out/estimation_2_20231110.rds")
  } else {
    sim <- readRDS("SimEngine.out/estimation_1_20231110.rds")
  }
  
  # Summarize results
  summ_bias <- summ_sd <- summ_cov <- list()
  for (i in c(1:51)) {
    m <- format(round(i/50-0.02,2), nsmall=2)
    summ_bias[[i]] <- list(
      stat = "bias",
      name = paste0("bias_",m),
      estimate = paste0("r_Mn_",m),
      truth = paste0("r_M0_",m)
      # na.rm = T
    )
    summ_sd[[i]] <- list(
      stat = "sd",
      name = paste0("sd_",m),
      x = paste0("r_Mn_",m)
      # na.rm = T
    )
    summ_cov[[i]] <- list(
      stat = "coverage",
      name = paste0("cov_",m),
      truth = paste0("r_M0_",m),
      lower = paste0("ci_lo_",m),
      upper = paste0("ci_up_",m)
      # na.rm = T
    )
  }
  
  names(sim$results) <- gsub("_hi", "_up", names(sim$results))
  
  summ_metrics <- c(summ_bias, summ_sd, summ_cov)
  summ <- do.call(SimEngine::summarize, c(list(sim), summ_metrics))
  
  summ %<>% rename("Estimator"=estimator)
  
  p_data <- pivot_longer(
    data = summ,
    cols = -c(level_id,n,alpha_3,sc_params,distr_S,edge,
              surv_true,sampling,Estimator,dir,wts_type),
    # cols = -c(level_id,n,alpha_3,sc_params,distr_S,edge,n_reps,
    #           surv_true,sampling,Estimator,dir,wts_type,use_package),
    names_to = c("stat","point"),
    names_sep = "_"
  )
  p_data %<>% mutate(point = as.numeric(point))
  
  # Plot Y-axis limits
  plot_lims <- list(b=c(-0.25,0.25), c=c(0,1), m=c(0,0.02),
                    v=c(0,0.01), s=c(0,0.15))
  # if (edge) { plot_lims$b <- c(-0.3,0.3) }
  
  # Set faceting vectors
  surv_trues <- c("Linear", "Cubic", "S-shaped")
  distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")
  if (edge) {
    surv_trues <- c("Linear", "Step")
    distr_Ss <- "N(0.5,0.04)"
  }
  
  # Orange 10/90 quantile lines
  df_vlines <- data.frame(
    x = c(qunif(0.1,0,1), qtruncnorm(0.1, a=0, b=1, mean=0.5, sd=0.2),
          qunif(0.9,0,1), qtruncnorm(0.9, a=0, b=1, mean=0.5, sd=0.2)),
    distr_S = rep(distr_Ss,2)
  )
  
  # Grey background densities
  if (edge) {
    df_distr_S <- data.frame(
      x = seq(0,1,0.01),
      ymax = dtruncnorm(seq(0,1,0.01), a=0, b=1, mean=0.5, sd=0.2),
      distr_S = rep(distr_Ss, each=101),
      value = 0
    )
  } else {
    df_distr_S <- data.frame(
      x = rep(seq(0,1,0.01),2),
      ymax = c(rep(1,101),
               dtruncnorm(seq(0,1,0.01), a=0, b=1, mean=0.5, sd=0.2)),
      distr_S = rep(distr_Ss, each=101),
      value = 0
    )
  }
  
  if (sim$levels$edge!="none") {
    mass <- as.numeric(strsplit(sim$levels$edge," ",fixed=T)[[1]][2])
    height <- 10 * mass
    df_distr_S %<>% mutate(
      ymax = ifelse(x<0.1, 10*mass, (1-mass)*df_distr_S$ymax)
    )
    df_vlines <- df_vlines[4:6,]
  }
  df_distr_b <- mutate(df_distr_S, ymin=plot_lims$b[1],
                       ymax=((ymax*diff(plot_lims$b))/6+plot_lims$b[1]))
  df_distr_c <- mutate(df_distr_S, ymin=plot_lims$c[1],
                       ymax=((ymax*diff(plot_lims$c))/6+plot_lims$c[1]))
  df_distr_s <- mutate(df_distr_S, ymin=plot_lims$s[1],
                       ymax=((ymax*diff(plot_lims$s))/6+plot_lims$s[1]))
  
  p_data %<>% mutate(
    surv_true = ifelse(surv_true=="Cox PH", "Linear", surv_true)
  )
  
  # Set up facets
  f_rows <- dplyr::vars(factor(distr_S, levels=distr_Ss))
  f_cols <- dplyr::vars(factor(surv_true, levels=surv_trues))
  
  # Set up plot objects
  p_aes <- aes(x=point, y=value, color=factor(Estimator),
               group=factor(Estimator))
  p_ribbon <- function(d) {
    geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, color=NA, group=NA),
                data=d, fill="grey", color=NA, alpha=0.4)
  }
  
  # Bias plot
  # Export: 10" x 6" (Cox_edge: 7" x 4")
  plot_b <- ggplot(filter(p_data, stat=="bias"), p_aes) +
    p_ribbon(df_distr_b) +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(limits=plot_lims$b) +
    theme(legend.position="bottom") +
    labs(y="Bias", x="S", color="Estimator")
  
  # Coverage plot
  # Export: 10" x 6" (Cox_edge: 7" x 4")
  plot_c <- ggplot(filter(p_data, stat=="cov"), p_aes) +
    p_ribbon(df_distr_c) +
    geom_hline(aes(yintercept=0.95), linetype="longdash", color="grey") +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(labels=percent, limits=plot_lims$c) +
    theme(legend.position="bottom") +
    labs(y="Coverage (%)", x="S", color="Estimator")
  
  # Standard error plot
  plot_s <- ggplot(filter(p_data, stat=="sd"), p_aes) +
    p_ribbon(df_distr_s) +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(limits=plot_lims$s) +
    theme(legend.position="bottom") +
    labs(y="Standard error", x="S", color="Estimator")
  
  # Save plots
  if (edge) {
    ggsave(filename="sim_est_edge_bias.pdf", plot=plot_b,
           device="pdf", width=7, height=4)
    ggsave(filename="sim_est_edge_cov.pdf", plot=plot_c,
           device="pdf", width=7, height=4)
    ggsave(filename="sim_est_edge_se.pdf", plot=plot_s,
           device="pdf", width=7, height=4)
  } else {
    ggsave(filename="sim_est_bias.pdf", plot=plot_b,
           device="pdf", width=10, height=6)
    ggsave(filename="sim_est_cov.pdf", plot=plot_c,
           device="pdf", width=10, height=6)
    ggsave(filename="sim_est_se.pdf", plot=plot_s,
           device="pdf", width=10, height=6)
  }
  
}



########################################################.
##### VIZ: Variance estimation (Cox paper), scaled #####
########################################################.

# Figures produced: se_est

sim <- readRDS("SimEngine.out/estimation_3_20230602.rds")

# Summarize results
summ_mean <- list()
summ_sd <- list()
for (i in c(1:4)) {
  pt <- c(1,11,26,41)[i]
  m <- format(round(pt/50-0.02,2), nsmall=2)
  summ_mean[[i]] <- list(
    stat = "mean",
    name = paste0("estimatedSE_",m),
    x = paste0("se_",m)
  )
  summ_sd[[i]] <- list(
    stat = "sd",
    name = paste0("empiricalSE_",m),
    x = paste0("r_Mn_",m)
  )
}
summ_metrics <- c(summ_mean, summ_sd)
summ <- do.call(SimEngine::summarize, c(list(sim), summ_metrics))

p_data <- pivot_longer(
  data = summ,
  # cols = -c(level_id,n,alpha_3,sc_params,distr_S,edge,return_se,n_reps,
  #           surv_true,sampling,estimator,dir,wts_type,use_package),
  cols = -c(level_id,n,alpha_3,sc_params,distr_S,edge,return_se,n_reps,
            surv_true,sampling,estimator,dir,wts_type),
  names_to = c("stat","point"),
  names_sep = "_"
)
p_data %<>% mutate(point = as.numeric(point))

# Plot Y-axis limits
plot_lims <- list(s=c(0,1.3))

# Set faceting vectors
surv_trues <- c("Linear", "Cubic", "S-shaped")
distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")

# Orange 10/90 quantile lines
df_vlines <- data.frame(
  x = c(qunif(0.1,0,1), qtruncnorm(0.1, a=0, b=1, mean=0.5, sd=0.2),
        qunif(0.9,0,1), qtruncnorm(0.9, a=0, b=1, mean=0.5, sd=0.2)),
  distr_S = rep(distr_Ss,2)
)

# Grey background densities
df_distr_S <- data.frame(
  x = rep(seq(0,1,0.01),2),
  ymax = c(rep(1,101),
           dtruncnorm(seq(0,1,0.01), a=0, b=1, mean=0.5, sd=0.2)),
  distr_S = rep(distr_Ss, each=101),
  value = 0
)

df_distr_s <- mutate(df_distr_S, ymin=plot_lims$s[1],
                     ymax=((ymax*diff(plot_lims$s))/6+plot_lims$s[1]))

p_data %<>% mutate(
  surv_true = ifelse(surv_true=="Cox PH", "Linear", surv_true),
  stat = ifelse(stat=="estimatedSE", "Estimated SE",
                ifelse(stat=="empiricalSE", "Empirical SE", "error"))
)

# Set up facets
f_rows <- dplyr::vars(factor(distr_S, levels=distr_Ss))
f_cols <- dplyr::vars(factor(surv_true, levels=surv_trues))

p_data %<>% filter(point==0.2)

# Set up plot objects
p_ribbon <- function(d) {
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax, color=NA, group=NA),
              data=d, fill="grey", color=NA, alpha=0.4)
}

# Scale values by root-n
p_data %<>% mutate(value=value*sqrt(n))

# Generate and save plot
plot <- ggplot(p_data, aes(x=n, y=value, color=factor(stat),
                           group=factor(stat))) +
  p_ribbon(df_distr_s) +
  geom_line() +
  geom_point(alpha=0.5) +
  facet_grid(rows=f_rows, cols=f_cols) +
  scale_y_continuous(limits=plot_lims$s) +
  # scale_color_manual(values=m_colors) +
  theme(legend.position="bottom") +
  labs(y="Scaled standard error", x="Sample size", color="")
ggsave(filename="se_est.pdf", plot=plot, device="pdf", width=10, height=6)



#########################################.
##### VIZ: Sample paths (Cox paper) #####
#########################################.

# Figures produced: sample_paths

if (F) {
  
  sim <- readRDS("SimEngine.out/estimation_1_20231110.rds")
  
  plot_df <- data.frame(
    "surv_true" = character(),
    "estimator" = character(),
    "row" = integer(),
    "point" = double(),
    "value" = double(),
    "which" = character()
  )
  
  res <- sim$results %>% mutate(
    surv_true = ifelse(surv_true=="Cox PH", "Linear", surv_true)
  )
  
  surv_trues <- c("Linear", "Cubic", "S-shaped")
  estimators <- c("Cox (basic)", "Cox (spline 4 df)", "Cox (spline 8 df)")
  
  # Add sample paths
  n_rows <- 10
  for (s in surv_trues) {
    for (e in estimators) {
      
      res2 <- filter(res, distr_S=="Unif(0,1)" & surv_true==s & estimator==e)
      res2 <- res2[1:n_rows,]
      
      for (row in c(1:n_rows)) {
        for (i in c(1:51)) {
          m <- format(round(i/50-0.02,2), nsmall=2)
          new_row <- list(
            "surv_true" = s,
            "estimator" = e,
            "row" = row,
            "point" = as.numeric(m),
            "value" = as.numeric(res2[row,paste0("r_Mn_",m)]),
            "which" = "Estimate"
          )
          plot_df[nrow(plot_df)+1,] <- new_row
        }
      }
      
    }
  }
  
  # Add true curves
  for (s in surv_trues) {
    for (e in estimators) {
      
      res2 <- filter(res, distr_S=="Unif(0,1)" & surv_true==s & estimator==e)
      res2 <- res2[1,]
      
      for (i in c(1:51)) {
        m <- format(round(i/50-0.02,2), nsmall=2)
        new_row <- list(
          "surv_true" = s,
          "estimator" = e,
          "row" = 0,
          "point" = as.numeric(m),
          "value" = as.numeric(res2[1,paste0("r_M0_",m)]),
          "which" = "Truth"
        )
        plot_df[nrow(plot_df)+1,] <- new_row
      }
      
    }
  }
  
  plot_df %<>% mutate(width=ifelse(which=="Estimate", 1, 2))
  
  # Generate and save plot
  plot <- ggplot(
    plot_df,
    aes(x=point, y=value, group=factor(row), color=which, alpha=which,
        linewidth=width)
  ) +
    geom_line() +
    scale_color_manual(values=c("forestgreen","black")) +
    scale_alpha_discrete(guide="none", range=c(0.5,1)) +
    scale_linewidth(guide="none", range=c(0.5,1)) +
    facet_grid(rows = dplyr::vars(factor(surv_true, levels=surv_trues)),
               cols = dplyr::vars(factor(estimator, levels=estimators))) +
    theme(legend.position="bottom") +
    labs(y="Marginalized risk", x="S", color="")
  ggsave(filename="sample_paths.pdf", plot=plot,
         device="pdf", width=10, height=6)
  
}
