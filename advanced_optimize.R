#!/usr/bin/env Rscript

# Advanced Parameter Optimization - Phase 2
# Focuses on cluster multipliers and advanced features

library(dplyr)
source("reimbursement_functions.R")

# Load training data
cat("Loading training data for advanced optimization...\n")
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

# Start with current best parameters
best_params <- c(
  75.0,   # base_per_diem
  0.6,    # mileage_rate_1
  0.5,    # mileage_rate_2
  0.4,    # mileage_rate_3
  0.7,    # receipt_rate_1
  0.4,    # receipt_rate_2
  0.6,    # receipt_rate_3
  0.1,    # receipt_rate_4
  1.0,    # cluster_mult_0
  1.0,    # cluster_mult_1
  1.0,    # cluster_mult_2
  1.0,    # cluster_mult_3
  1.0,    # cluster_mult_4
  1.0,    # cluster_mult_5
  1.0,    # vacation_bonus_mult
  1.0,    # vacation_base_mult
  1.0,    # five_day_mult
  1.0,    # receipt_threshold_mult
  1.0,    # efficiency_threshold_mult
  1.0,    # miles_threshold_mult
  0.0,    # interaction_coeff
  0.0,    # log_bonus_coeff
  0.0,    # bug_base_bonus
  0.0     # bug_multiplier
)

best_error <- objective_function(best_params)
cat("Starting error: $", round(best_error, 2), "\n")

# Analyze cluster distribution to understand which clusters need adjustment
cat("\nAnalyzing cluster distribution...\n")
cluster_analysis <- data.frame(
  trip_duration = test_data$trip_duration_days,
  miles_traveled = test_data$miles_traveled,
  receipts = test_data$total_receipts_amount,
  expected = test_data$expected_output
)

# Calculate current predictions and cluster assignments
current_predictions <- numeric(nrow(test_data))
cluster_assignments <- numeric(nrow(test_data))

for (i in 1:nrow(test_data)) {
  features <- engineer_features(test_data$trip_duration_days[i], 
                               test_data$miles_traveled[i], 
                               test_data$total_receipts_amount[i])
  cluster_assignments[i] <- determine_cluster(features)
  current_predictions[i] <- calculate_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i]
  )
}

# Analyze errors by cluster
cluster_errors <- data.frame(
  cluster = cluster_assignments,
  expected = test_data$expected_output,
  predicted = current_predictions,
  error = abs(current_predictions - test_data$expected_output)
)

cluster_summary <- cluster_errors %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    avg_expected = mean(expected),
    avg_predicted = mean(predicted),
    avg_error = mean(error),
    ratio = mean(predicted) / mean(expected),
    .groups = 'drop'
  )

cat("Cluster analysis:\n")
print(cluster_summary)

# Optimize cluster multipliers based on the ratio analysis
cat("\nOptimizing cluster multipliers...\n")
for (cluster in 0:5) {
  cluster_data <- cluster_summary[cluster_summary$cluster == cluster, ]
  if (nrow(cluster_data) > 0) {
    # Adjust multiplier based on ratio
    current_ratio <- cluster_data$ratio
    target_ratio <- 1.0  # We want predicted/expected = 1
    adjustment_factor <- target_ratio / current_ratio
    
    # Apply adjustment with some dampening to avoid overcorrection
    new_multiplier <- best_params[9 + cluster] * (0.7 + 0.3 * adjustment_factor)
    
    # Test the new multiplier
    test_params <- best_params
    test_params[9 + cluster] <- new_multiplier
    error <- objective_function(test_params)
    
    if (error < best_error) {
      best_error <- error
      best_params <- test_params
      cat("  Cluster", cluster, "multiplier:", round(new_multiplier, 3), "error:", round(error, 2), "\n")
    }
  }
}

# Fine-tune base parameters
cat("\nFine-tuning base parameters...\n")

# Optimize base per diem more precisely
for (per_diem in seq(best_params[1] - 10, best_params[1] + 10, 2)) {
  test_params <- best_params
  test_params[1] <- per_diem
  error <- objective_function(test_params)
  if (error < best_error) {
    best_error <- error
    best_params <- test_params
    cat("  New best per diem:", per_diem, "error:", round(error, 2), "\n")
  }
}

# Optimize receipt rates more precisely
cat("Fine-tuning receipt rates...\n")
for (rate1 in seq(best_params[5] - 0.2, best_params[5] + 0.2, 0.05)) {
  for (rate2 in seq(best_params[6] - 0.2, best_params[6] + 0.2, 0.05)) {
    for (rate3 in seq(best_params[7] - 0.2, best_params[7] + 0.2, 0.05)) {
      test_params <- best_params
      test_params[5:7] <- c(rate1, rate2, rate3)
      error <- objective_function(test_params)
      if (error < best_error) {
        best_error <- error
        best_params <- test_params
        cat("  New best receipt rates:", round(rate1, 2), round(rate2, 2), round(rate3, 2), "error:", round(error, 2), "\n")
      }
    }
  }
}

# Test some interaction effects
cat("\nTesting interaction effects...\n")
for (interaction in seq(-0.001, 0.001, 0.0002)) {
  test_params <- best_params
  test_params[21] <- interaction
  error <- objective_function(test_params)
  if (error < best_error) {
    best_error <- error
    best_params <- test_params
    cat("  New best interaction coeff:", interaction, "error:", round(error, 2), "\n")
  }
}

# Test log bonus effects
cat("\nTesting log bonus effects...\n")
for (log_bonus in seq(-5, 5, 1)) {
  test_params <- best_params
  test_params[22] <- log_bonus
  error <- objective_function(test_params)
  if (error < best_error) {
    best_error <- error
    best_params <- test_params
    cat("  New best log bonus:", log_bonus, "error:", round(error, 2), "\n")
  }
}

# Test threshold multipliers
cat("\nTesting threshold multipliers...\n")
for (i in 17:20) {  # five_day, receipt_threshold, efficiency_threshold, miles_threshold
  for (mult in seq(0.8, 1.2, 0.05)) {
    test_params <- best_params
    test_params[i] <- mult
    error <- objective_function(test_params)
    if (error < best_error) {
      best_error <- error
      best_params <- test_params
      param_names <- c("five_day", "receipt_threshold", "efficiency_threshold", "miles_threshold")
      cat("  New best", param_names[i-16], "multiplier:", round(mult, 3), "error:", round(error, 2), "\n")
    }
  }
}

cat("\n=== ADVANCED OPTIMIZATION RESULTS ===\n")
cat("Final average error: $", round(best_error, 2), "\n")

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
cat("\n=== FINAL OPTIMIZED PARAMETERS ===\n")
for (i in 1:length(best_params)) {
  cat(sprintf("%-25s: %8.5f\n", param_names[i], best_params[i]))
}

# Generate updated config function
new_config_code <- sprintf('
# Advanced optimized parameters
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

cat("\nNew config function:\n")
cat(new_config_code)

cat("\n\nAdvanced optimization complete!\n")
cat("Update reimbursement_functions.R with the new config and run eval.R to test.\n")
