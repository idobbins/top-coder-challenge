#!/usr/bin/env Rscript

# Quick setup and run script for parameter optimization

cat("=== REIMBURSEMENT SYSTEM PARAMETER OPTIMIZATION ===\n\n")

# Check and install required packages
required_packages <- c("dplyr", "jsonlite", "parallel", "GA")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
  }
}

# Load packages
suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(parallel)
  library(GA)
})

cat("All required packages loaded successfully!\n\n")

# Source and run the optimization
cat("Starting parameter optimization...\n")
cat("This may take 30-60 minutes depending on your system.\n")
cat("Progress will be shown as the optimization runs.\n\n")

source("advanced_parameter_optimizer.R")

# Run the main optimization
results <- main_optimization(strategies = c("genetic"), max_iterations = 200)

cat("\n=== OPTIMIZATION COMPLETE ===\n")
cat("Check the 'optimized_config.R' file for your new parameters.\n")
cat("Run eval.R again to see the improved results!\n")
