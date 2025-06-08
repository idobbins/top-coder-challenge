#!/usr/bin/env Rscript

# Quick Parameter Optimization for Immediate Improvement
# Focuses on the most impactful parameters first

library(dplyr)
source("reimbursement_functions.R")

# Load training data
cat("Loading training data for quick optimization...\n")
test_data <- read.csv("public_cases.csv")

# Objective function for optimization
objective_function <- function(params) {
  # Update config with new parameters
  config <- list(
    base_per_diem = params[1],
    mileage_rates = c(params[2], params[3], params[4]),
    receipt_rates = c(params[5], params[6], params[7], params[8]),
    cluster_multipliers = c(params[9], params[10], params[11], params[12], params[13], params[14]),
    vacation_bonus_multiplier = params[15],
    vacation_base_multiplier = params[16],
    five_day_multiplier = params[17],
    receipt_threshold_multiplier = params[18],
    efficiency_threshold_multiplier = params[19],
    miles_threshold_multiplier = params[20],
    interaction_coeff = params[21],
    log_bonus_coeff = params[22],
    bug_base_bonus = params[23],
    bug_multiplier = params[24]
  )
  
  errors <- numeric(nrow(test_data))
  
  for (i in 1:nrow(test_data)) {
    tryCatch({
      prediction <- calculate_reimbursement(
        test_data$trip_duration_days[i],
        test_data$miles_traveled[i],
        test_data$total_receipts_amount[i],
        config
      )
      errors[i] <- abs(prediction - test_data$expected_output[i])
    }, error = function(e) {
      errors[i] <- 1000.0  # Large penalty for errors
    })
  }
  
  return(mean(errors))
}

# Start with simpler baseline parameters (closer to typical reimbursement rates)
baseline_params <- c(
  50,    # base_per_diem (more reasonable)
  0.5,   # mileage_rate_1 (standard IRS rate)
  0.4,   # mileage_rate_2
  0.3,   # mileage_rate_3
  0.8,   # receipt_rate_1 (higher reimbursement for small amounts)
  0.6,   # receipt_rate_2
  0.4,   # receipt_rate_3
  0.2,   # receipt_rate_4
  1.0,   # cluster_mult_0
  1.0,   # cluster_mult_1
  1.0,   # cluster_mult_2
  1.0,   # cluster_mult_3
  1.0,   # cluster_mult_4
  1.0,   # cluster_mult_5
  1.0,   # vacation_bonus_mult
  1.0,   # vacation_base_mult
  1.0,   # five_day_mult
  1.0,   # receipt_threshold_mult
  1.0,   # efficiency_threshold_mult
  1.0,   # miles_threshold_mult
  0.0,   # interaction_coeff (start with no interaction)
  0.0,   # log_bonus_coeff (start with no log bonus)
  0.0,   # bug_base_bonus (start with no bug)
  0.0    # bug_multiplier (start with no bug)
)

cat("Testing baseline parameters...\n")
baseline_error <- objective_function(baseline_params)
cat("Baseline average error: $", round(baseline_error, 2), "\n")

# Simple grid search on key parameters
cat("\nOptimizing key parameters...\n")

best_params <- baseline_params
best_error <- baseline_error

# Optimize base per diem
cat("Optimizing base per diem...\n")
for (per_diem in seq(30, 80, 5)) {
  test_params <- best_params
  test_params[1] <- per_diem
  error <- objective_function(test_params)
  if (error < best_error) {
    best_error <- error
    best_params <- test_params
    cat("  New best per diem:", per_diem, "error:", round(error, 2), "\n")
  }
}

# Optimize mileage rates
cat("Optimizing mileage rates...\n")
for (rate1 in seq(0.3, 0.7, 0.1)) {
  for (rate2 in seq(0.2, 0.6, 0.1)) {
    for (rate3 in seq(0.1, 0.5, 0.1)) {
      test_params <- best_params
      test_params[2:4] <- c(rate1, rate2, rate3)
      error <- objective_function(test_params)
      if (error < best_error) {
        best_error <- error
        best_params <- test_params
        cat("  New best mileage rates:", rate1, rate2, rate3, "error:", round(error, 2), "\n")
      }
    }
  }
}

# Optimize receipt rates
cat("Optimizing receipt rates...\n")
for (rate1 in seq(0.5, 1.0, 0.1)) {
  for (rate2 in seq(0.3, 0.8, 0.1)) {
    for (rate3 in seq(0.2, 0.6, 0.1)) {
      for (rate4 in seq(0.1, 0.4, 0.1)) {
        test_params <- best_params
        test_params[5:8] <- c(rate1, rate2, rate3, rate4)
        error <- objective_function(test_params)
        if (error < best_error) {
          best_error <- error
          best_params <- test_params
          cat("  New best receipt rates:", rate1, rate2, rate3, rate4, "error:", round(error, 2), "\n")
        }
      }
    }
  }
}

cat("\n=== OPTIMIZATION RESULTS ===\n")
cat("Best average error: $", round(best_error, 2), "\n")
cat("Improvement: $", round(baseline_error - best_error, 2), "\n")

# Parameter names for output
param_names <- c(
  "base_per_diem", "mileage_rate_1", "mileage_rate_2", "mileage_rate_3",
  "receipt_rate_1", "receipt_rate_2", "receipt_rate_3", "receipt_rate_4",
  "cluster_mult_0", "cluster_mult_1", "cluster_mult_2", "cluster_mult_3",
  "cluster_mult_4", "cluster_mult_5", "vacation_bonus_mult", "vacation_base_mult",
  "five_day_mult", "receipt_threshold_mult", "efficiency_threshold_mult",
  "miles_threshold_mult", "interaction_coeff", "log_bonus_coeff",
  "bug_base_bonus", "bug_multiplier"
)

# Display optimized parameters
cat("\n=== OPTIMIZED PARAMETERS ===\n")
for (i in 1:length(best_params)) {
  cat(sprintf("%-25s: %8.5f\n", param_names[i], best_params[i]))
}

# Update the reimbursement_functions.R file with new parameters
cat("\n=== UPDATING REIMBURSEMENT FUNCTIONS ===\n")

new_config_code <- sprintf('
# Optimized parameters from quick_optimize.R
get_config <- function() {
  return(list(
    base_per_diem = %.5f,
    mileage_rates = c(%.5f, %.5f, %.5f),
    receipt_rates = c(%.5f, %.5f, %.5f, %.5f),
    cluster_multipliers = c(%.5f, %.5f, %.5f, %.5f, %.5f, %.5f),
    vacation_bonus_multiplier = %.5f,
    vacation_base_multiplier = %.5f,
    five_day_multiplier = %.5f,
    receipt_threshold_multiplier = %.5f,
    efficiency_threshold_multiplier = %.5f,
    miles_threshold_multiplier = %.5f,
    interaction_coeff = %.8f,
    log_bonus_coeff = %.5f,
    bug_base_bonus = %.5f,
    bug_multiplier = %.5f
  ))
}',
best_params[1], best_params[2], best_params[3], best_params[4],
best_params[5], best_params[6], best_params[7], best_params[8],
best_params[9], best_params[10], best_params[11], best_params[12], best_params[13], best_params[14],
best_params[15], best_params[16], best_params[17], best_params[18], best_params[19], best_params[20],
best_params[21], best_params[22], best_params[23], best_params[24])

cat("New config function:\n")
cat(new_config_code)

cat("\n\nQuick optimization complete!\n")
cat("Run 'Rscript eval.R' to test the improved performance.\n")
