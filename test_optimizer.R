#!/usr/bin/env Rscript

# Quick test script to verify the optimizer works before running full optimization

cat("=== TESTING PARAMETER OPTIMIZER ===\n\n")

# Load required libraries (install if needed)
required_packages <- c("dplyr", "jsonlite", "parallel")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
  }
}

suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(parallel)
})

# Source the functions
source("reimbursement_functions.R")
source("advanced_parameter_optimizer.R")

# Test basic functionality
cat("Testing data loading...\n")
test_data <- load_test_data()
cat("Loaded", nrow(test_data), "test cases successfully.\n\n")

cat("Testing parameter bounds...\n")
bounds <- get_parameter_bounds()
cat("Defined", length(bounds), "parameters for optimization.\n\n")

cat("Testing config conversion...\n")
# Create a test parameter vector
test_params <- numeric(length(bounds))
for (i in 1:length(bounds)) {
  # Use middle value of each bound
  test_params[i] <- mean(bounds[[i]])
}

test_config <- vector_to_config(test_params)
cat("Successfully converted parameter vector to config.\n\n")

cat("Testing fitness function...\n")
GLOBAL_TEST_DATA <<- test_data[1:10, ]  # Use just 10 cases for quick test
test_fitness <- fitness_function(test_params)
cat("Fitness function returned:", test_fitness, "\n\n")

cat("Testing detailed evaluation...\n")
baseline_config <- get_config()
baseline_results <- detailed_evaluation(baseline_config)
cat("Baseline evaluation completed.\n")
cat("  Exact matches:", baseline_results$exact_matches, "/", nrow(GLOBAL_TEST_DATA), "\n")
cat("  Average error: $", round(baseline_results$avg_error, 2), "\n\n")

cat("=== ALL TESTS PASSED ===\n")
cat("The optimizer is ready to run!\n")
cat("Execute 'Rscript run_optimization.R' to start the full optimization.\n")
