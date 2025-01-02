#################.
##### Setup #####
#################.

cb_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
               "#0072B2", "#D55E00", "#F0E442", "#999999")



####################################.
##### Estimation (bias,cov,sd) #####
####################################.

# Figures produced: sim_est_edge_bias, sim_est_edge_cov, sim_est_edge_se,
#                   sim_est_bias, sim_est_cov, sim_est_se

for (edge in c(F,T)) {
  
  if (edge) {
    sim <- readRDS("SimEngine.out/estimation_2_20231110.rds")
  } else {
    sim <- readRDS("SimEngine.out/estimation_1_20231110.rds")
  }
  
  # Summarize results
  summ_bias <- summ_sd <- summ_cov <- summ_mse <- list()
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
    summ_mse[[i]] <- list(
      stat = "mse",
      name = paste0("mse_",m),
      estimate = paste0("r_Mn_",m),
      truth = paste0("r_M0_",m)
      # na.rm = T
    )
  }
  
  names(sim$results) <- gsub("_hi", "_up", names(sim$results))
  
  summ_metrics <- c(summ_bias, summ_sd, summ_cov, summ_mse)
  summ <- do.call(SimEngine::summarize, c(list(sim), summ_metrics))
  
  p_data <- pivot_longer(
    data = summ,
    cols = -c("level_id", "n_reps", names(sim$levels)), # "mean_num_events"
    names_to = c("stat","point"),
    names_sep = "_"
  )
  p_data %<>% mutate(point = as.numeric(point))
  
  p_data %<>% rename("Estimator"=estimator)
  
  # Plot Y-axis limits
  plot_lims <- list(b=c(-0.25,0.25), c=c(0,1), m=c(0,0.03),
                    v=c(0,0.01), s=c(0,0.15))
  # if (edge) { plot_lims$b <- c(-0.3,0.3) }
  
  # Set faceting vectors
  surv_trues <- c("Linear", "Cubic", "S-shaped")
  distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")
  if (edge) {
    surv_trues <- c("Linear", "Step")
    distr_Ss <- "N(0.5,0.04)"
  }
  
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
  }
  df_distr_b <- mutate(df_distr_S, ymin=plot_lims$b[1],
                       ymax=((ymax*diff(plot_lims$b))/6+plot_lims$b[1]))
  df_distr_c <- mutate(df_distr_S, ymin=plot_lims$c[1],
                       ymax=((ymax*diff(plot_lims$c))/6+plot_lims$c[1]))
  df_distr_s <- mutate(df_distr_S, ymin=plot_lims$s[1],
                       ymax=((ymax*diff(plot_lims$s))/6+plot_lims$s[1]))
  df_distr_m <- mutate(df_distr_S, ymin=plot_lims$m[1],
                       ymax=((ymax*diff(plot_lims$m))/6+plot_lims$m[1]))
  
  if (edge) {
    ests <- c("Cox (basic)", "Cox (spline 4 df)", "Cox (edge)",
              "Cox (edge + spline 4 df)")
  } else {
    ests <- c("Cox (basic)", "Cox (spline 4 df)", "Cox (spline 8 df)")
  }
  
  p_data %<>% mutate(
    surv_true = ifelse(surv_true=="Cox PH", "Linear", surv_true),
    Estimator = factor(Estimator, levels=ests)
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
  plot_b <- ggplot(filter(p_data, stat=="bias"), p_aes) +
    p_ribbon(df_distr_b) +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(limits=plot_lims$b) +
    scale_color_manual(values=cb_colors) +
    theme(legend.position="bottom") +
    labs(y="Bias", x="S", color="Estimator")
  
  # Coverage plot
  plot_c <- ggplot(filter(p_data, stat=="cov"), p_aes) +
    p_ribbon(df_distr_c) +
    geom_hline(aes(yintercept=0.95), linetype="longdash", color="grey") +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(labels=percent, limits=plot_lims$c) +
    scale_color_manual(values=cb_colors) +
    theme(legend.position="bottom") +
    labs(y="Coverage (%)", x="S", color="Estimator")
  
  # Standard error plot
  plot_s <- ggplot(filter(p_data, stat=="sd"), p_aes) +
    p_ribbon(df_distr_s) +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(limits=plot_lims$s) +
    scale_color_manual(values=cb_colors) +
    theme(legend.position="bottom") +
    labs(y="Standard error", x="S", color="Estimator")
  
  # MSE plot
  plot_m <- ggplot(filter(p_data, stat=="mse"), p_aes) +
    p_ribbon(df_distr_m) +
    geom_line() +
    facet_grid(rows=f_rows, cols=f_cols) +
    scale_y_continuous(limits=plot_lims$m) +
    scale_color_manual(values=cb_colors) +
    theme(legend.position="bottom") +
    labs(y="Mean squared error", x="S", color="Estimator")
  
  # Save plots
  if (edge) {
    ggsave(filename="sim_est_edge_bias.pdf", plot=plot_b,
           device="pdf", width=7, height=4)
    ggsave(filename="sim_est_edge_cov.pdf", plot=plot_c,
           device="pdf", width=7, height=4)
    ggsave(filename="sim_est_edge_se.pdf", plot=plot_s,
           device="pdf", width=7, height=4)
    ggsave(filename="sim_est_edge_mse.pdf", plot=plot_m,
           device="pdf", width=7, height=4)
  } else {
    ggsave(filename="sim_est_bias.pdf", plot=plot_b,
           device="pdf", width=10, height=6)
    ggsave(filename="sim_est_cov.pdf", plot=plot_c,
           device="pdf", width=10, height=6)
    ggsave(filename="sim_est_se.pdf", plot=plot_s,
           device="pdf", width=10, height=6)
    ggsave(filename="sim_est_mse.pdf", plot=plot_m,
           device="pdf", width=10, height=6)
  }
  
}



###################################################.
##### Variance estimation (Cox paper), scaled #####
###################################################.

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
  cols = -c("level_id", "n_reps", names(sim$levels)), # "mean_num_events"
  names_to = c("stat","point"),
  names_sep = "_"
)
p_data %<>% mutate(point = as.numeric(point))

# Plot Y-axis limits
plot_lims <- list(s=c(0,1.3))

# Set faceting vectors
surv_trues <- c("Linear", "Cubic", "S-shaped")
distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")

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
  scale_color_manual(values=cb_colors) +
  theme(legend.position="bottom") +
  labs(y="Scaled standard error", x="Sample size", color="")
ggsave(filename="se_est.pdf", plot=plot, device="pdf", width=10, height=6)



####################################.
##### Sample paths (Cox paper) #####
####################################.

# Figures produced: sample_paths

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
  scale_color_manual(values=c("#009E73","black")) +
  scale_alpha_discrete(guide="none", range=c(0.5,1)) +
  scale_linewidth(guide="none", range=c(0.5,1)) +
  facet_grid(rows = dplyr::vars(factor(surv_true, levels=surv_trues)),
             cols = dplyr::vars(factor(estimator, levels=estimators))) +
  theme(legend.position="bottom") +
  labs(y="Marginalized risk", x="S", color="")
ggsave(filename="sample_paths.pdf", plot=plot,
       device="pdf", width=10, height=6)



#############################################.
##### Comparison to bootstrap: coverage #####
#############################################.

# Figures produced: bootstrap_comp_coverage

sim <- readRDS("SimEngine.out/estimation_4_20231203.rds")

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

# # Coverage
# summ[,c(1,3,6,7,13,16,17,26,36,46,56,66,76)] %>% arrange(sc_params, distr_S, bootstrap)

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

# Set up facets
f_cols <- dplyr::vars(factor(distr_S, levels=distr_Ss))

p_data %<>% dplyr::filter(point==0.5)
p_data %<>% dplyr::mutate(
  bootstrap = ifelse(bootstrap, "Bootstrap", "Analytic"),
  num_events = case_when(
    sc_params=="lmbd_23" & distr_S=="Unif(0,1)" ~ 9.9,
    sc_params=="lmbd_23" & distr_S=="N(0.5,0.04)" ~ 9.2,
    # sc_params=="lmbd_24" & distr_S=="Unif(0,1)" ~ 19.6,
    # sc_params=="lmbd_24" & distr_S=="N(0.5,0.04)" ~ 18.2,
    sc_params=="lmbd_25" & distr_S=="Unif(0,1)" ~ 38.4,
    sc_params=="lmbd_25" & distr_S=="N(0.5,0.04)" ~ 35.1,
    sc_params=="lmbd_26" & distr_S=="Unif(0,1)" ~ 72.2,
    sc_params=="lmbd_26" & distr_S=="N(0.5,0.04)" ~ 67.5,
    sc_params=="lmbd_27" & distr_S=="Unif(0,1)" ~ 133.4,
    sc_params=="lmbd_27" & distr_S=="N(0.5,0.04)" ~ 126.7,
    sc_params=="lmbd_32" & distr_S=="Unif(0,1)" ~ 230.1,
    sc_params=="lmbd_32" & distr_S=="N(0.5,0.04)" ~ 222.1
  )
)

# Coverage plot
plot <- ggplot(
  p_data,
  aes(x=num_events, y=value, color=factor(bootstrap),
      group=factor(bootstrap))) +
  geom_hline(aes(yintercept=0.95), linetype="longdash", color="grey") +
  geom_line() +
  facet_grid(cols=f_cols) +
  scale_y_continuous(labels=percent, limits=c(0.7,1)) +
  scale_color_manual(values=cb_colors) +
  theme(legend.position="bottom") +
  labs(y="Coverage (%)", x="Expected number of events", color=NULL)
ggsave(filename="bootstrap_comp_coverage.pdf", plot=plot, device="pdf",
       width=6, height=3.5)



#############################################.
##### Comparison to bootstrap: CI width #####
#############################################.

# Figures produced: bootstrap_comp_ci_width

sim <- readRDS("SimEngine.out/estimation_4_20231122.rds")

for (i in c(1:51)) {
  m <- format(round(i/50-0.02,2), nsmall=2)
  sim$results[,paste0("ciwd_",m)] <- sim$results[,paste0("ci_up_",m)] -
    sim$results[,paste0("ci_lo_",m)]
}
summ <- sim %>% SimEngine::summarize(
  list(stat="mean", x="ciwd_0.20", name="ciwd_0.20", na.rm=T),
  list(stat="mean", x="ciwd_0.50", name="ciwd_0.50", na.rm=T),
  list(stat="mean", x="ciwd_0.80", name="ciwd_0.80", na.rm=T)
)

p_data <- pivot_longer(
  data = summ,
  cols = -c("level_id", "n_reps", names(sim$levels)),
  names_to = c("stat","point"),
  names_sep = "_"
)
p_data %<>% mutate(point = as.numeric(point))

# Set faceting vectors
distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")

# Set limits
p_lims <- list(x=c(0,150), y=c(0,0.04))

# Set up facets
f_cols <- dplyr::vars(factor(distr_S, levels=distr_Ss))

p_data %<>% dplyr::filter(point==0.5)
p_data %<>% dplyr::mutate(
  bootstrap = ifelse(bootstrap, "Bootstrap", "Analytic"),
  num_events = case_when(
    sc_params=="lmbd_23" & distr_S=="Unif(0,1)" ~ 9.9,
    sc_params=="lmbd_23" & distr_S=="N(0.5,0.04)" ~ 9.2,
    # sc_params=="lmbd_24" & distr_S=="Unif(0,1)" ~ 19.6,
    # sc_params=="lmbd_24" & distr_S=="N(0.5,0.04)" ~ 18.2,
    sc_params=="lmbd_25" & distr_S=="Unif(0,1)" ~ 38.4,
    sc_params=="lmbd_25" & distr_S=="N(0.5,0.04)" ~ 35.1,
    sc_params=="lmbd_26" & distr_S=="Unif(0,1)" ~ 72.2,
    sc_params=="lmbd_26" & distr_S=="N(0.5,0.04)" ~ 67.5,
    sc_params=="lmbd_27" & distr_S=="Unif(0,1)" ~ 133.4,
    sc_params=="lmbd_27" & distr_S=="N(0.5,0.04)" ~ 126.7,
    sc_params=="lmbd_32" & distr_S=="Unif(0,1)" ~ 230.1,
    sc_params=="lmbd_32" & distr_S=="N(0.5,0.04)" ~ 222.1
  )
)

# CI width plot
plot <- ggplot(
  p_data,
  aes(x=num_events, y=value, color=factor(bootstrap),
      group=factor(bootstrap))) +
  geom_line() +
  facet_grid(cols=f_cols) +
  # ylim(p_lims$y) +
  # xlim(p_lims$x) +
  theme(legend.position="bottom") +
  scale_color_manual(values=cb_colors) +
  labs(y="Confidence interval width", x="Expected number of events", color=NULL)
ggsave(filename="bootstrap_comp_ci_width.pdf", plot=plot, device="pdf",
       width=6, height=3.5)



####################################.
##### Confidence band coverage #####
####################################.

# Figures produced: uniform_conf_bands

sim <- readRDS("SimEngine.out/estimation_5_20250102.rds")

# Summarize results
cov_vec <- rep(1, nrow(sim$results))
for (i in c(1:51)) {
  m <- format(round(i/50-0.02,2), nsmall=2)
  cov_vec <- cov_vec * as.integer(
    sim$results[[paste0("ci_lo_",m)]] <= sim$results[[paste0("r_M0_",m)]] &
      sim$results[[paste0("r_M0_",m)]] <= sim$results[[paste0("ci_up_",m)]]
  )
}
mean(cov_vec)
# !!!!! CONTINUE
# sim %>% SimEngine::summarize(
#   list(stat="mean", x="cov_unif")
# )








# for (i in c(1:51)) {
#   m <- format(round(i/50-0.02,2), nsmall=2)
#   sim$results[,paste0("ciwd_",m)] <- sim$results[,paste0("ci_up_",m)] -
#     sim$results[,paste0("ci_lo_",m)]
# }
# summ <- sim %>% SimEngine::summarize(
#   list(stat="mean", x="ciwd_0.20", name="ciwd_0.20", na.rm=T),
#   list(stat="mean", x="ciwd_0.50", name="ciwd_0.50", na.rm=T),
#   list(stat="mean", x="ciwd_0.80", name="ciwd_0.80", na.rm=T)
# )
# 
# p_data <- pivot_longer(
#   data = summ,
#   cols = -c("level_id", "n_reps", names(sim$levels)),
#   names_to = c("stat","point"),
#   names_sep = "_"
# )
# p_data %<>% mutate(point = as.numeric(point))
# 
# # Set faceting vectors
# distr_Ss <- c("Unif(0,1)", "N(0.5,0.04)")
# 
# # Set limits
# p_lims <- list(x=c(0,150), y=c(0,0.04))
# 
# # Set up facets
# f_cols <- dplyr::vars(factor(distr_S, levels=distr_Ss))
# 
# p_data %<>% dplyr::filter(point==0.5)
# p_data %<>% dplyr::mutate(
#   bootstrap = ifelse(bootstrap, "Bootstrap", "Analytic"),
#   num_events = case_when(
#     sc_params=="lmbd_23" & distr_S=="Unif(0,1)" ~ 9.9,
#     sc_params=="lmbd_23" & distr_S=="N(0.5,0.04)" ~ 9.2,
#     # sc_params=="lmbd_24" & distr_S=="Unif(0,1)" ~ 19.6,
#     # sc_params=="lmbd_24" & distr_S=="N(0.5,0.04)" ~ 18.2,
#     sc_params=="lmbd_25" & distr_S=="Unif(0,1)" ~ 38.4,
#     sc_params=="lmbd_25" & distr_S=="N(0.5,0.04)" ~ 35.1,
#     sc_params=="lmbd_26" & distr_S=="Unif(0,1)" ~ 72.2,
#     sc_params=="lmbd_26" & distr_S=="N(0.5,0.04)" ~ 67.5,
#     sc_params=="lmbd_27" & distr_S=="Unif(0,1)" ~ 133.4,
#     sc_params=="lmbd_27" & distr_S=="N(0.5,0.04)" ~ 126.7,
#     sc_params=="lmbd_32" & distr_S=="Unif(0,1)" ~ 230.1,
#     sc_params=="lmbd_32" & distr_S=="N(0.5,0.04)" ~ 222.1
#   )
# )
# 
# # CI width plot
# plot <- ggplot(
#   p_data,
#   aes(x=num_events, y=value, color=factor(bootstrap),
#       group=factor(bootstrap))) +
#   geom_line() +
#   facet_grid(cols=f_cols) +
#   # ylim(p_lims$y) +
#   # xlim(p_lims$x) +
#   theme(legend.position="bottom") +
#   scale_color_manual(values=cb_colors) +
#   labs(y="Confidence interval width", x="Expected number of events", color=NULL)
# ggsave(filename="bootstrap_comp_ci_width.pdf", plot=plot, device="pdf",
#        width=6, height=3.5)







