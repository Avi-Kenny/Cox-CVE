# Change library path
.libPaths(c("/home/akenny/R_lib", .libPaths()))

# Set packages
# GitHub packages: tedwestling/ctsCausal, tedwestling/CFsurvival,
#                  tedwestling/survSuperLearner, zeehio/facetscales
cfg$pkgs <- c(
  "vaccine", "dplyr", "boot", "car", "mgcv", "memoise", "EnvStats", "fdrtool",
  "splines", "survival", "SuperLearner", "survSuperLearner", "randomForestSRC",
  "CFsurvival", "Rsolnp", "truncnorm", "tidyr", "ranger", "survey", "pbapply",
  "compiler", "simest" # "survML" "xgboost"
)
cfg$pkgs_nocluster <- c(
  "ggplot2", "viridis", "sqldf", "scales", "data.table", "latex2exp"
)

# Set cluster config
if (Sys.getenv("HOME")=="/home/akenny") {
  # Bionic
  cluster_config <- list(
    js = "slurm",
    dir = paste0("/home/akenny/", Sys.getenv("project"))
  )
} else if (Sys.getenv("HOME")=="/home/users/avikenny") {
  # Bayes
  cluster_config <- list(
    js = "ge",
    dir = paste0("/home/users/avikenny/Desktop/", Sys.getenv("project"))
  )
} else {
  cluster_config <- list(js="", dir="")
}

# Load packages (if running locally or not running sims)
if (Sys.getenv("USERDOMAIN")=="AVI-KENNY-T460" || !cfg$run_sims) {
  for (pkg in c(cfg$pkgs,cfg$pkgs_nocluster)) {
    suppressMessages({ do.call("library", list(pkg)) })
  }
}
