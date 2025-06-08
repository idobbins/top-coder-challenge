#!/usr/bin/env Rscript

# Phase 1 Focused Optimization - Quick parameter tuning for immediate gains
# Focus on the most impactful new parameters

library(dplyr)
source("enhanced_reimbursement_functions.R")

# Load training data
cat("=== PHASE 1 OPTIMIZATION STARTING ===\n")
cat("Loading training data...\n")
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

# Objective function for optimization
objective_function <- function(config_updates) {
  # Start with current config
  config <- get_config()
  
  # Apply updates
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
      errors[i] <- 1000.0  # Large penalty for errors
    })
  }
  
  return(mean(errors))
}

# Get baseline performance with enhanced features
baseline_config <- get_config()
baseline_error <- objective_function(list())
cat("Enhanced baseline average error: $", round(baseline_error, 2), "\n")

best_config <- baseline_config
best_error <- baseline_error

# =============================================================================
# PHASE 1: FOCUSED PARAMETER OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 1: FOCUSED PARAMETER OPTIMIZATION ===\n")

# Optimize enhanced bug parameters
cat("Optimizing enhanced bug parameters...\n")
best_bug_base <- best_config$bug_base_bonus
best_bug_mult <- best_config$bug_multiplier

for (base_bonus in seq(10, 25, 2.5)) {
  for (multiplier in seq(0.005, 0.015, 0.002)) {
    error <- objective_function(list(bug_base_bonus = base_bonus, bug_multiplier = multiplier))
    
    if (error < best_error) {
      improvement <- best_error - error
      best_error <- error
      best_config$bug_base_bonus <- base_bonus
      best_config$bug_multiplier <- multiplier
      cat("  New best bug params:", base_bonus, multiplier, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
    }
  }
}

# Optimize log bonus coefficient
cat("Optimizing log bonus coefficient...\n")
for (coeff in seq(8, 18, 1)) {
  error <- objective_function(list(log_bonus_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$log_bonus_coeff <- coeff
    cat("  New best log bonus coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

# Optimize interaction coefficient
cat("Optimizing interaction coefficient...\n")
for (coeff in seq(-0.002, 0.004, 0.0005)) {
  error <- objective_function(list(interaction_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$interaction_coeff <- coeff
    cat("  New best interaction coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

# Optimize new feature coefficients
cat("Optimizing polynomial coefficient...\n")
for (coeff in seq(0.00005, 0.0002, 0.00002)) {
  error <- objective_function(list(polynomial_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$polynomial_coeff <- coeff
    cat("  New best polynomial coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

cat("Optimizing threshold proximity bonus...\n")
for (bonus in seq(2, 10, 1)) {
  error <- objective_function(list(threshold_proximity_bonus = bonus))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$threshold_proximity_bonus <- bonus
    cat("  New best proximity bonus:", bonus, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

cat("Optimizing efficiency ratio coefficient...\n")
for (coeff in seq(0.5, 4, 0.5)) {
  error <- objective_function(list(efficiency_ratio_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$efficiency_ratio_coeff <- coeff
    cat("  New best efficiency ratio coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

cat("Optimizing pattern bonus coefficient...\n")
for (coeff in seq(4, 15, 1)) {
  error <- objective_function(list(pattern_bonus_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$pattern_bonus_coeff <- coeff
    cat("  New best pattern bonus coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

# Quick optimization of key existing parameters
cat("Quick optimization of key existing parameters...\n")

# Optimize base per diem
for (per_diem in seq(best_config$base_per_diem - 3, best_config$base_per_diem + 3, 1)) {
  error <- objective_function(list(base_per_diem = per_diem))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$base_per_diem <- per_diem
    cat("  New best per diem:", per_diem, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

# Optimize five day multiplier
for (mult in seq(1.05, 1.15, 0.01)) {
  error <- objective_function(list(five_day_multiplier = mult))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$five_day_multiplier <- mult
    cat("  New best five day multiplier:", mult, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  }
}

# =============================================================================
# RESULTS AND OUTPUT
# =============================================================================
cat("\n=== PHASE 1 OPTIMIZATION RESULTS ===\n")
cat("Starting error: $", round(baseline_error, 2), "\n")
cat("Final error: $", round(best_error, 2), "\n")
cat("Total improvement: $", round(baseline_error - best_error, 2), "\n")
cat("Improvement percentage:", round((baseline_error - best_error) / baseline_error * 100, 1), "%\n")

# Generate updated get_config function
cat("\n=== UPDATED GET_CONFIG FUNCTION ===\n")
cat("get_config <- function() {\n")
cat("  return(list(\n")
cat("    # Basic calculation parameters\n")
cat("    base_per_diem =", best_config$base_per_diem, ",\n")
cat("    mileage_rates = c(", paste(round(best_config$mileage_rates, 5), collapse = ", "), "),\n")
cat("    receipt_rates = c(", paste(round(best_config$receipt_rates, 5), collapse = ", "), "),\n")
cat("    \n")
cat("    # Tier boundaries\n")
cat("    mileage_tiers = c(", paste(best_config$mileage_tiers, collapse = ", "), "),\n")
cat("    receipt_tiers = c(", paste(best_config$receipt_tiers, collapse = ", "), "),\n")
cat("    \n")
cat("    # Threshold values\n")
cat("    receipt_threshold =", best_config$receipt_threshold, ",\n")
cat("    efficiency_threshold =", best_config$efficiency_threshold, ",\n")
cat("    miles_threshold =", best_config$miles_threshold, ",\n")
cat("    \n")
cat("    # Feature engineering thresholds\n")
cat("    efficiency_zones = c(", paste(best_config$efficiency_zones, collapse = ", "), "),\n")
cat("    spending_categories = c(", paste(best_config$spending_categories, collapse = ", "), "),\n")
cat("    sweet_spot_duration = c(", paste(best_config$sweet_spot_duration, collapse = ", "), "),\n")
cat("    \n")
cat("    # Cluster definition parameters\n")
cat("    cluster_boundaries = list(\n")
cat("      very_short_high_eff = c(", paste(best_config$cluster_boundaries$very_short_high_eff, collapse = ", "), "),\n")
cat("      long_low_eff = c(", paste(best_config$cluster_boundaries$long_low_eff, collapse = ", "), "),\n")
cat("      medium_high_spend = c(", paste(best_config$cluster_boundaries$medium_high_spend, collapse = ", "), "),\n")
cat("      medium_high_eff = c(", paste(best_config$cluster_boundaries$medium_high_eff, collapse = ", "), "),\n")
cat("      long_high_spend = c(", paste(best_config$cluster_boundaries$long_high_spend, collapse = ", "), ")\n")
cat("    ),\n")
cat("    \n")
cat("    # Multipliers and adjustments\n")
cat("    cluster_multipliers = c(", paste(round(best_config$cluster_multipliers, 5), collapse = ", "), "),\n")
cat("    vacation_bonus_multiplier =", round(best_config$vacation_bonus_multiplier, 5), ",\n")
cat("    vacation_base_multiplier =", round(best_config$vacation_base_multiplier, 5), ",\n")
cat("    five_day_multiplier =", round(best_config$five_day_multiplier, 5), ",\n")
cat("    receipt_threshold_multiplier =", round(best_config$receipt_threshold_multiplier, 5), ",\n")
cat("    efficiency_threshold_multiplier =", round(best_config$efficiency_threshold_multiplier, 5), ",\n")
cat("    miles_threshold_multiplier =", round(best_config$miles_threshold_multiplier, 5), ",\n")
cat("    \n")
cat("    # Enhanced advanced features\n")
cat("    interaction_coeff =", best_config$interaction_coeff, ",\n")
cat("    log_bonus_coeff =", best_config$log_bonus_coeff, ",\n")
cat("    bug_base_bonus =", best_config$bug_base_bonus, ",\n")
cat("    bug_multiplier =", best_config$bug_multiplier, ",\n")
cat("    \n")
cat("    # Enhanced rounding bug parameters\n")
cat("    bug_cents_values = c(", paste(best_config$bug_cents_values, collapse = ", "), "),\n")
cat("    \n")
cat("    # New parameters for enhanced features\n")
cat("    polynomial_coeff =", best_config$polynomial_coeff, ",\n")
cat("    threshold_proximity_bonus =", best_config$threshold_proximity_bonus, ",\n")
cat("    efficiency_ratio_coeff =", best_config$efficiency_ratio_coeff, ",\n")
cat("    pattern_bonus_coeff =", best_config$pattern_bonus_coeff, "\n")
cat("  ))\n")
cat("}\n")

cat("\n=== PHASE 1 OPTIMIZATION COMPLETE ===\n")
cat("Copy the updated get_config function above into enhanced_reimbursement_functions.R\n")
cat("Then test with 'Rscript eval.R' using the enhanced functions!\n")
