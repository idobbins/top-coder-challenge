#!/usr/bin/env Rscript

# Phase 2 Parameter Optimizer
# Optimizes the pattern-specific adjustments from Phase 2 analysis

suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(GA)
})

# Source the enhanced functions
source("phase2_enhanced_functions.R")

cat("=== PHASE 2 PATTERN ADJUSTMENT OPTIMIZER ===\n\n")

# Global variables for optimization
GLOBAL_TEST_DATA <- NULL
GLOBAL_BEST_SCORE <- Inf
GLOBAL_BEST_CONFIG <- NULL
GLOBAL_ITERATION <- 0

# Load test data
load_test_data <- function() {
  test_data <- jsonlite::fromJSON("public_cases.json")
  return(data.frame(
    trip_duration_days = test_data$input$trip_duration_days,
    miles_traveled = test_data$input$miles_traveled,
    total_receipts_amount = test_data$input$total_receipts_amount,
    expected_output = test_data$expected_output
  ))
}

# Evaluation function for Phase 2 adjustments
evaluate_phase2_adjustments <- function(adjustments) {
  test_data <- GLOBAL_TEST_DATA
  n_cases <- nrow(test_data)
  
  # Create config with new adjustments
  config <- get_phase2_config()
  config$pattern_adjustments$receipt_49_99_penalty <- adjustments[1]
  config$pattern_adjustments$receipt_0x_bonus <- adjustments[2]
  config$pattern_adjustments$extreme_efficiency_penalty <- adjustments[3]
  config$pattern_adjustments$very_low_efficiency_penalty <- adjustments[4]
  config$pattern_adjustments$extreme_spending_penalty <- adjustments[5]
  config$pattern_adjustments$very_high_spending_penalty <- adjustments[6]
  config$pattern_adjustments$one_day_trip_penalty <- adjustments[7]
  config$pattern_adjustments$short_trip_penalty <- adjustments[8]
  config$pattern_adjustments$one_day_long_distance_penalty <- adjustments[9]
  config$pattern_adjustments$short_high_expense_bonus <- adjustments[10]
  
  # Calculate predictions
  predictions <- numeric(n_cases)
  for (i in 1:n_cases) {
    predictions[i] <- calculate_phase2_reimbursement(
      test_data$trip_duration_days[i],
      test_data$miles_traveled[i],
      test_data$total_receipts_amount[i],
      config
    )
  }
  
  # Calculate score
  errors <- abs(predictions - test_data$expected_output)
  score <- sum(errors^2)
  
  return(score)
}

# Fitness function for genetic algorithm
fitness_function_phase2 <- function(params) {
  GLOBAL_ITERATION <<- GLOBAL_ITERATION + 1
  
  score <- evaluate_phase2_adjustments(params)
  
  # Track best configuration
  if (score < GLOBAL_BEST_SCORE) {
    GLOBAL_BEST_SCORE <<- score
    GLOBAL_BEST_CONFIG <<- params
    
    # Calculate detailed metrics for best config
    test_data <- GLOBAL_TEST_DATA
    config <- get_phase2_config()
    config$pattern_adjustments$receipt_49_99_penalty <- params[1]
    config$pattern_adjustments$receipt_0x_bonus <- params[2]
    config$pattern_adjustments$extreme_efficiency_penalty <- params[3]
    config$pattern_adjustments$very_low_efficiency_penalty <- params[4]
    config$pattern_adjustments$extreme_spending_penalty <- params[5]
    config$pattern_adjustments$very_high_spending_penalty <- params[6]
    config$pattern_adjustments$one_day_trip_penalty <- params[7]
    config$pattern_adjustments$short_trip_penalty <- params[8]
    config$pattern_adjustments$one_day_long_distance_penalty <- params[9]
    config$pattern_adjustments$short_high_expense_bonus <- params[10]
    
    predictions <- numeric(nrow(test_data))
    for (i in 1:nrow(test_data)) {
      predictions[i] <- calculate_phase2_reimbursement(
        test_data$trip_duration_days[i],
        test_data$miles_traveled[i],
        test_data$total_receipts_amount[i],
        config
      )
    }
    
    errors <- abs(predictions - test_data$expected_output)
    exact_matches <- sum(errors <= 0.01)
    avg_error <- mean(errors)
    
    cat("New best score:", round(score, 2), "(Exact:", exact_matches, ", Avg Error: $", round(avg_error, 2), ")\n")
  }
  
  # Progress tracking
  if (GLOBAL_ITERATION %% 50 == 0) {
    cat("Iteration", GLOBAL_ITERATION, ", Current best:", round(GLOBAL_BEST_SCORE, 2), "\n")
  }
  
  return(-score)  # GA maximizes, we want to minimize
}

# Parameter bounds for Phase 2 adjustments
get_phase2_bounds <- function() {
  return(list(
    receipt_49_99_penalty = c(0.85, 1.15),        # More conservative range
    receipt_0x_bonus = c(0.95, 1.10),             # Smaller bonus range
    extreme_efficiency_penalty = c(0.90, 1.10),   # More conservative
    very_low_efficiency_penalty = c(0.90, 1.10),  # More conservative
    extreme_spending_penalty = c(0.85, 1.15),     # Moderate range
    very_high_spending_penalty = c(0.90, 1.10),   # Conservative
    one_day_trip_penalty = c(0.85, 1.05),         # Less aggressive penalty
    short_trip_penalty = c(0.90, 1.05),           # Conservative
    one_day_long_distance_penalty = c(0.85, 1.05), # Less aggressive
    short_high_expense_bonus = c(0.95, 1.15)      # Moderate bonus
  ))
}

# Save optimized Phase 2 configuration
save_phase2_config <- function(best_params, filename = "phase2_optimized_config.R") {
  config_content <- paste0(
    "# Phase 2 Optimized Configuration\n",
    "# Generated on: ", Sys.time(), "\n\n",
    "get_phase2_optimized_config <- function() {\n",
    "  base_config <- get_optimized_config()\n",
    "  enhanced_config <- base_config\n\n",
    "  # Optimized Phase 2 pattern adjustments\n",
    "  enhanced_config$pattern_adjustments <- list(\n",
    "    receipt_49_99_penalty = ", best_params[1], ",\n",
    "    receipt_0x_bonus = ", best_params[2], ",\n",
    "    extreme_efficiency_penalty = ", best_params[3], ",\n",
    "    very_low_efficiency_penalty = ", best_params[4], ",\n",
    "    extreme_spending_penalty = ", best_params[5], ",\n",
    "    very_high_spending_penalty = ", best_params[6], ",\n",
    "    one_day_trip_penalty = ", best_params[7], ",\n",
    "    short_trip_penalty = ", best_params[8], ",\n",
    "    one_day_long_distance_penalty = ", best_params[9], ",\n",
    "    short_high_expense_bonus = ", best_params[10], "\n",
    "  )\n\n",
    "  return(enhanced_config)\n",
    "}\n"
  )
  
  writeLines(config_content, filename)
  cat("Saved optimized Phase 2 configuration to", filename, "\n")
}

# Main optimization function
main_phase2_optimization <- function() {
  cat("Loading test data and initializing...\n")
  GLOBAL_TEST_DATA <<- load_test_data()
  
  cat("Evaluating baseline Phase 2 configuration...\n")
  baseline_score <- evaluate_phase2_adjustments(c(0.75, 1.05, 0.80, 0.85, 0.78, 0.88, 0.70, 0.85, 0.75, 1.08))
  cat("Baseline Phase 2 score:", round(baseline_score, 2), "\n\n")
  
  # Reset global tracking
  GLOBAL_BEST_SCORE <<- Inf
  GLOBAL_BEST_CONFIG <<- NULL
  GLOBAL_ITERATION <<- 0
  
  cat("Running Phase 2 pattern adjustment optimization...\n")
  
  bounds <- get_phase2_bounds()
  lower_bounds <- sapply(bounds, function(x) x[1])
  upper_bounds <- sapply(bounds, function(x) x[2])
  
  # Run genetic algorithm
  result <- ga(
    type = "real-valued",
    fitness = fitness_function_phase2,
    lower = lower_bounds,
    upper = upper_bounds,
    popSize = 40,
    maxiter = 100,
    run = 20,
    parallel = FALSE,
    monitor = TRUE,
    seed = 456
  )
  
  cat("\nPhase 2 optimization completed!\n")
  
  # Final evaluation
  if (!is.null(GLOBAL_BEST_CONFIG)) {
    cat("\n=== FINAL PHASE 2 OPTIMIZATION RESULTS ===\n")
    
    # Create optimized config
    config <- get_phase2_config()
    config$pattern_adjustments$receipt_49_99_penalty <- GLOBAL_BEST_CONFIG[1]
    config$pattern_adjustments$receipt_0x_bonus <- GLOBAL_BEST_CONFIG[2]
    config$pattern_adjustments$extreme_efficiency_penalty <- GLOBAL_BEST_CONFIG[3]
    config$pattern_adjustments$very_low_efficiency_penalty <- GLOBAL_BEST_CONFIG[4]
    config$pattern_adjustments$extreme_spending_penalty <- GLOBAL_BEST_CONFIG[5]
    config$pattern_adjustments$very_high_spending_penalty <- GLOBAL_BEST_CONFIG[6]
    config$pattern_adjustments$one_day_trip_penalty <- GLOBAL_BEST_CONFIG[7]
    config$pattern_adjustments$short_trip_penalty <- GLOBAL_BEST_CONFIG[8]
    config$pattern_adjustments$one_day_long_distance_penalty <- GLOBAL_BEST_CONFIG[9]
    config$pattern_adjustments$short_high_expense_bonus <- GLOBAL_BEST_CONFIG[10]
    
    # Calculate final metrics
    test_data <- GLOBAL_TEST_DATA
    predictions <- numeric(nrow(test_data))
    for (i in 1:nrow(test_data)) {
      predictions[i] <- calculate_phase2_reimbursement(
        test_data$trip_duration_days[i],
        test_data$miles_traveled[i],
        test_data$total_receipts_amount[i],
        config
      )
    }
    
    errors <- abs(predictions - test_data$expected_output)
    exact_matches <- sum(errors <= 0.01)
    close_matches <- sum(errors <= 1.00)
    avg_error <- mean(errors)
    median_error <- median(errors)
    max_error <- max(errors)
    rmse <- sqrt(mean(errors^2))
    score <- sum(errors^2)
    
    cat("Optimized Phase 2 Results:\n")
    cat("  Exact matches:", exact_matches, "/", nrow(test_data), 
        "(", round(exact_matches/nrow(test_data)*100, 1), "%)\n")
    cat("  Close matches:", close_matches, "/", nrow(test_data),
        "(", round(close_matches/nrow(test_data)*100, 1), "%)\n")
    cat("  Average error: $", round(avg_error, 2), "\n")
    cat("  Median error: $", round(median_error, 2), "\n")
    cat("  Max error: $", round(max_error, 2), "\n")
    cat("  RMSE: $", round(rmse, 2), "\n")
    cat("  Score:", round(score, 2), "\n\n")
    
    # Improvement summary
    baseline_errors <- abs(GLOBAL_TEST_DATA$expected_output - 109.80)  # Approximate from Phase 1
    improvement_avg_error <- mean(baseline_errors) - avg_error
    improvement_score <- baseline_score - score
    
    cat("Improvements from baseline:\n")
    cat("  Average error: -$", round(improvement_avg_error, 2), "\n")
    cat("  Score improvement:", round(improvement_score, 2), "\n\n")
    
    # Save optimized configuration
    save_phase2_config(GLOBAL_BEST_CONFIG)
    
    cat("Phase 2 optimization complete!\n")
  } else {
    cat("No improvement found in Phase 2 optimization.\n")
  }
}

# Run the optimization
main_phase2_optimization()
