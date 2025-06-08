#!/usr/bin/env Rscript

# Simple optimization runner without parallel processing

cat("=== REIMBURSEMENT SYSTEM PARAMETER OPTIMIZATION ===\n\n")

# Load required libraries
suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(GA)
})

cat("All required packages loaded successfully!\n\n")

# Source the functions
source("reimbursement_functions.R")
source("advanced_parameter_optimizer.R")

cat("Starting parameter optimization (non-parallel version)...\n")
cat("This will take 20-30 minutes.\n")
cat("Progress will be shown as the optimization runs.\n\n")

# Load test data
GLOBAL_TEST_DATA <<- load_test_data()

# Get baseline performance
cat("Evaluating baseline configuration...\n")
baseline_config <- get_config()
baseline_results <- detailed_evaluation(baseline_config)

cat("Baseline Results:\n")
cat("  Exact matches:", baseline_results$exact_matches, "/", nrow(GLOBAL_TEST_DATA), "\n")
cat("  Average error: $", round(baseline_results$avg_error, 2), "\n")
cat("  Score:", round(baseline_results$score, 2), "\n\n")

# Reset global tracking
GLOBAL_BEST_SCORE <<- Inf
GLOBAL_BEST_CONFIG <<- NULL
GLOBAL_ITERATION <<- 0

# Run optimization
cat("Running genetic algorithm optimization...\n")

bounds <- get_parameter_bounds()
lower_bounds <- sapply(bounds, function(x) x[1])
upper_bounds <- sapply(bounds, function(x) x[2])

# Run GA without parallel processing
result <- ga(
  type = "real-valued",
  fitness = fitness_function,
  lower = lower_bounds,
  upper = upper_bounds,
  popSize = 30,
  maxiter = 100,
  run = 25,
  parallel = FALSE,  # Disable parallel processing
  monitor = TRUE,
  seed = 123
)

cat("\nOptimization completed!\n")

# Final evaluation of best configuration
if (!is.null(GLOBAL_BEST_CONFIG)) {
  cat("\n=== FINAL OPTIMIZATION RESULTS ===\n")
  final_results <- detailed_evaluation(GLOBAL_BEST_CONFIG)
  
  cat("Optimized Results:\n")
  cat("  Exact matches:", final_results$exact_matches, "/", nrow(GLOBAL_TEST_DATA), 
      "(", round(final_results$exact_matches/nrow(GLOBAL_TEST_DATA)*100, 1), "%)\n")
  cat("  Close matches:", final_results$close_matches, "/", nrow(GLOBAL_TEST_DATA),
      "(", round(final_results$close_matches/nrow(GLOBAL_TEST_DATA)*100, 1), "%)\n")
  cat("  Average error: $", round(final_results$avg_error, 2), "\n")
  cat("  Median error: $", round(final_results$median_error, 2), "\n")
  cat("  Max error: $", round(final_results$max_error, 2), "\n")
  cat("  RMSE: $", round(final_results$rmse, 2), "\n")
  cat("  Score:", round(final_results$score, 2), "\n\n")
  
  # Improvement summary
  improvement_exact <- final_results$exact_matches - baseline_results$exact_matches
  improvement_avg_error <- baseline_results$avg_error - final_results$avg_error
  improvement_score <- baseline_results$score - final_results$score
  
  cat("Improvements:\n")
  cat("  Exact matches: +", improvement_exact, "\n")
  cat("  Average error: -$", round(improvement_avg_error, 2), "\n")
  cat("  Score improvement:", round(improvement_score, 2), "\n\n")
  
  # Save optimized configuration
  save_optimized_config(GLOBAL_BEST_CONFIG)
  
  cat("Optimization complete! Check 'optimized_config.R' for the new configuration.\n")
  cat("To use the optimized config, replace get_config() with get_optimized_config() in your code.\n")
} else {
  cat("No improvement found. Try running with more iterations.\n")
}
