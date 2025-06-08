#!/usr/bin/env Rscript

# Quick optimization of conservative parameters
library(dplyr)
source("conservative_enhanced.R")

# Load training data
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

# Objective function
objective_function <- function(config_updates) {
  config <- get_config()
  for (name in names(config_updates)) {
    config[[name]] <- config_updates[[name]]
  }
  
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
      errors[i] <- 1000.0
    })
  }
  return(mean(errors))
}

# Get baseline
baseline_error <- objective_function(list())
cat("Baseline error: $", round(baseline_error, 2), "\n")

best_config <- get_config()
best_error <- baseline_error

# Quick parameter sweep
cat("Optimizing conservative parameters...\n")

# Optimize interaction coefficient
for (coeff in seq(0, 0.002, 0.0002)) {
  error <- objective_function(list(interaction_coeff = coeff))
  if (error < best_error) {
    best_error <- error
    best_config$interaction_coeff <- coeff
    cat("  New best interaction_coeff:", coeff, "error:", round(error, 2), "\n")
  }
}

# Optimize log bonus coefficient
for (coeff in seq(0, 8, 1)) {
  error <- objective_function(list(log_bonus_coeff = coeff))
  if (error < best_error) {
    best_error <- error
    best_config$log_bonus_coeff <- coeff
    cat("  New best log_bonus_coeff:", coeff, "error:", round(error, 2), "\n")
  }
}

# Optimize bug parameters
for (base in seq(0, 10, 2)) {
  for (mult in seq(0, 0.005, 0.001)) {
    error <- objective_function(list(bug_base_bonus = base, bug_multiplier = mult))
    if (error < best_error) {
      best_error <- error
      best_config$bug_base_bonus <- base
      best_config$bug_multiplier <- mult
      cat("  New best bug params:", base, mult, "error:", round(error, 2), "\n")
    }
  }
}

# Optimize base per diem
for (per_diem in seq(57, 62, 0.5)) {
  error <- objective_function(list(base_per_diem = per_diem))
  if (error < best_error) {
    best_error <- error
    best_config$base_per_diem <- per_diem
    cat("  New best base_per_diem:", per_diem, "error:", round(error, 2), "\n")
  }
}

cat("\n=== OPTIMIZATION RESULTS ===\n")
cat("Starting error: $", round(baseline_error, 2), "\n")
cat("Final error: $", round(best_error, 2), "\n")
cat("Improvement: $", round(baseline_error - best_error, 2), "\n")

# Calculate final score
exact_matches <- 0  # Estimate
score <- best_error * 100 + (1000 - exact_matches) * 0.1
cat("Estimated score:", round(score, 2), "\n")

# Output optimized config
cat("\n=== OPTIMIZED CONSERVATIVE CONFIG ===\n")
cat("interaction_coeff =", best_config$interaction_coeff, "\n")
cat("log_bonus_coeff =", best_config$log_bonus_coeff, "\n")
cat("bug_base_bonus =", best_config$bug_base_bonus, "\n")
cat("bug_multiplier =", best_config$bug_multiplier, "\n")
cat("base_per_diem =", best_config$base_per_diem, "\n")
