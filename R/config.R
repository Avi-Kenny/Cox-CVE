# Change library path
.libPaths(c("/home/akenny/R_lib", "/hpc/home/ak811/R_lib", .libPaths()))

# Set packages
# GitHub packages: tedwestling/ctsCausal, tedwestling/CFsurvival,
#                  tedwestling/survSuperLearner, zeehio/facetscales
cfg$pkgs <- c(
  "vaccine", "dplyr", "boot", "car", "mgcv", "memoise", "EnvStats", "fdrtool",
  "splines", "survival", "SuperLearner", "survSuperLearner", "randomForestSRC",
  "Rsolnp", "truncnorm", "tidyr", "ranger", "survey", "pbapply",
  "compiler" # "survML" "xgboost" "CFsurvival"
)
cfg$pkgs_nocluster <- c(
  "ggplot2", "viridis", "sqldf", "scales", "data.table", "latex2exp"
)

# Set cluster config
if (Sys.getenv("HOME")=="/home/akenny") {
  # Bionic
  cluster_config <- list(
    js = "slurm",
    dir = paste0("/home/akenny/", Sys.getenv("proj"),
                 "/Code__", Sys.getenv("proj"))
  )
} else if (Sys.getenv("HOME")=="/hpc/home/ak811") {
  # Duke DCC
  cluster_config <- list(
    js = "slurm",
    dir = paste0("/hpc/home/ak811/", Sys.getenv("proj"),
                 "/Code__", Sys.getenv("proj"))
  )
} else if (Sys.getenv("HOME")=="/home/ak811") {
  # Duke RCC
  cluster_config <- list(
    js = "slurm",
    dir = paste0("/shared/home/ak811/", Sys.getenv("proj"),
                 "/Code__", Sys.getenv("proj"))
  )
} else {
  cluster_config <- list(js="", dir="")
}

# Load packages (if running locally or not running sims)
if (Sys.getenv("RSTUDIO")=="1" || !cfg$run_sims) {
  for (pkg in c(cfg$pkgs,cfg$pkgs_nocluster)) {
    suppressMessages({ do.call("library", list(pkg)) })
  }
}
