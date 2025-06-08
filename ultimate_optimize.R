#!/usr/bin/env Rscript

# Ultimate Parameter Optimization - Comprehensive Hyperparameter Tuning
# Combines threshold analysis, tier optimization, and advanced parameter tuning

library(dplyr)
source("reimbursement_functions.R")

# Load training data
cat("=== ULTIMATE OPTIMIZATION STARTING ===\n")
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

# Get baseline performance
baseline_config <- get_config()
baseline_error <- objective_function(list())
cat("Baseline average error: $", round(baseline_error, 2), "\n")

best_config <- baseline_config
best_error <- baseline_error

# =============================================================================
# PHASE 1: THRESHOLD OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 1: THRESHOLD OPTIMIZATION ===\n")

# Optimize receipt threshold
cat("Optimizing receipt threshold...\n")
receipt_thresholds <- seq(500, 800, 25)
for (thresh in receipt_thresholds) {
  error <- objective_function(list(receipt_threshold = thresh))
  if (error < best_error) {
    best_error <- error
    best_config$receipt_threshold <- thresh
    cat("  New best receipt threshold:", thresh, "error:", round(error, 2), "\n")
  }
}

# Optimize efficiency threshold
cat("Optimizing efficiency threshold...\n")
efficiency_thresholds <- seq(150, 220, 10)
for (thresh in efficiency_thresholds) {
  error <- objective_function(list(efficiency_threshold = thresh))
  if (error < best_error) {
    best_error <- error
    best_config$efficiency_threshold <- thresh
    cat("  New best efficiency threshold:", thresh, "error:", round(error, 2), "\n")
  }
}

# Optimize miles threshold
cat("Optimizing miles threshold...\n")
miles_thresholds <- seq(400, 600, 25)
for (thresh in miles_thresholds) {
  error <- objective_function(list(miles_threshold = thresh))
  if (error < best_error) {
    best_error <- error
    best_config$miles_threshold <- thresh
    cat("  New best miles threshold:", thresh, "error:", round(error, 2), "\n")
  }
}

cat("Phase 1 complete. Error reduced to: $", round(best_error, 2), "\n")

# =============================================================================
# PHASE 2: TIER BOUNDARY OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 2: TIER BOUNDARY OPTIMIZATION ===\n")

# Optimize mileage tiers
cat("Optimizing mileage tier boundaries...\n")
for (tier1 in seq(80, 120, 10)) {
  for (tier2 in seq(450, 550, 25)) {
    if (tier2 > tier1) {
      error <- objective_function(list(mileage_tiers = c(tier1, tier2)))
      if (error < best_error) {
        best_error <- error
        best_config$mileage_tiers <- c(tier1, tier2)
        cat("  New best mileage tiers:", tier1, tier2, "error:", round(error, 2), "\n")
      }
    }
  }
}

# Optimize receipt tiers
cat("Optimizing receipt tier boundaries...\n")
for (tier1 in seq(40, 60, 5)) {
  for (tier2 in seq(450, 550, 25)) {
    for (tier3 in seq(1400, 1600, 50)) {
      if (tier2 > tier1 && tier3 > tier2) {
        error <- objective_function(list(receipt_tiers = c(tier1, tier2, tier3)))
        if (error < best_error) {
          best_error <- error
          best_config$receipt_tiers <- c(tier1, tier2, tier3)
          cat("  New best receipt tiers:", tier1, tier2, tier3, "error:", round(error, 2), "\n")
        }
      }
    }
  }
}

cat("Phase 2 complete. Error reduced to: $", round(best_error, 2), "\n")

# =============================================================================
# PHASE 3: RATE OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 3: RATE OPTIMIZATION ===\n")

# Optimize base per diem
cat("Optimizing base per diem...\n")
for (per_diem in seq(best_config$base_per_diem - 10, best_config$base_per_diem + 10, 2)) {
  error <- objective_function(list(base_per_diem = per_diem))
  if (error < best_error) {
    best_error <- error
    best_config$base_per_diem <- per_diem
    cat("  New best per diem:", per_diem, "error:", round(error, 2), "\n")
  }
}

# Optimize mileage rates
cat("Optimizing mileage rates...\n")
current_rates <- best_config$mileage_rates
for (rate1 in seq(current_rates[1] - 0.1, current_rates[1] + 0.1, 0.02)) {
  for (rate2 in seq(current_rates[2] - 0.1, current_rates[2] + 0.1, 0.02)) {
    for (rate3 in seq(current_rates[3] - 0.1, current_rates[3] + 0.1, 0.02)) {
      error <- objective_function(list(mileage_rates = c(rate1, rate2, rate3)))
      if (error < best_error) {
        best_error <- error
        best_config$mileage_rates <- c(rate1, rate2, rate3)
        cat("  New best mileage rates:", round(rate1, 3), round(rate2, 3), round(rate3, 3), "error:", round(error, 2), "\n")
      }
    }
  }
}

# Optimize receipt rates
cat("Optimizing receipt rates...\n")
current_rates <- best_config$receipt_rates
for (rate1 in seq(current_rates[1] - 0.1, current_rates[1] + 0.1, 0.02)) {
  for (rate2 in seq(current_rates[2] - 0.1, current_rates[2] + 0.1, 0.02)) {
    for (rate3 in seq(current_rates[3] - 0.1, current_rates[3] + 0.1, 0.02)) {
      for (rate4 in seq(current_rates[4] - 0.05, current_rates[4] + 0.05, 0.01)) {
        error <- objective_function(list(receipt_rates = c(rate1, rate2, rate3, rate4)))
        if (error < best_error) {
          best_error <- error
          best_config$receipt_rates <- c(rate1, rate2, rate3, rate4)
          cat("  New best receipt rates:", round(rate1, 3), round(rate2, 3), round(rate3, 3), round(rate4, 3), "error:", round(error, 2), "\n")
        }
      }
    }
  }
}

cat("Phase 3 complete. Error reduced to: $", round(best_error, 2), "\n")

# =============================================================================
# PHASE 4: MULTIPLIER OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 4: MULTIPLIER OPTIMIZATION ===\n")

# Optimize cluster multipliers
cat("Optimizing cluster multipliers...\n")
current_mults <- best_config$cluster_multipliers
for (i in 1:length(current_mults)) {
  for (mult in seq(current_mults[i] - 0.05, current_mults[i] + 0.05, 0.01)) {
    new_mults <- current_mults
    new_mults[i] <- mult
    error <- objective_function(list(cluster_multipliers = new_mults))
    if (error < best_error) {
      best_error <- error
      best_config$cluster_multipliers <- new_mults
      cat("  New best cluster", i-1, "multiplier:", round(mult, 3), "error:", round(error, 2), "\n")
    }
  }
}

# Optimize threshold multipliers
cat("Optimizing threshold multipliers...\n")
threshold_multipliers <- c("five_day_multiplier", "receipt_threshold_multiplier", 
                          "efficiency_threshold_multiplier", "miles_threshold_multiplier")

for (mult_name in threshold_multipliers) {
  current_val <- best_config[[mult_name]]
  for (mult in seq(current_val - 0.1, current_val + 0.1, 0.02)) {
    update <- list()
    update[[mult_name]] <- mult
    error <- objective_function(update)
    if (error < best_error) {
      best_error <- error
      best_config[[mult_name]] <- mult
      cat("  New best", mult_name, ":", round(mult, 3), "error:", round(error, 2), "\n")
    }
  }
}

cat("Phase 4 complete. Error reduced to: $", round(best_error, 2), "\n")

# =============================================================================
# PHASE 5: ADVANCED FEATURE OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 5: ADVANCED FEATURE OPTIMIZATION ===\n")

# Optimize interaction coefficient
cat("Optimizing interaction coefficient...\n")
for (coeff in seq(-0.002, 0.002, 0.0002)) {
  error <- objective_function(list(interaction_coeff = coeff))
  if (error < best_error) {
    best_error <- error
    best_config$interaction_coeff <- coeff
    cat("  New best interaction coeff:", coeff, "error:", round(error, 2), "\n")
  }
}

# Optimize log bonus coefficient
cat("Optimizing log bonus coefficient...\n")
for (coeff in seq(-10, 10, 1)) {
  error <- objective_function(list(log_bonus_coeff = coeff))
  if (error < best_error) {
    best_error <- error
    best_config$log_bonus_coeff <- coeff
    cat("  New best log bonus coeff:", coeff, "error:", round(error, 2), "\n")
  }
}

# Optimize bug parameters
cat("Optimizing bug parameters...\n")
for (base_bonus in seq(0, 50, 5)) {
  for (multiplier in seq(0, 0.02, 0.002)) {
    error <- objective_function(list(bug_base_bonus = base_bonus, bug_multiplier = multiplier))
    if (error < best_error) {
      best_error <- error
      best_config$bug_base_bonus <- base_bonus
      best_config$bug_multiplier <- multiplier
      cat("  New best bug params:", base_bonus, multiplier, "error:", round(error, 2), "\n")
    }
  }
}

cat("Phase 5 complete. Error reduced to: $", round(best_error, 2), "\n")

# =============================================================================
# RESULTS AND OUTPUT
# =============================================================================
cat("\n=== ULTIMATE OPTIMIZATION RESULTS ===\n")
cat("Starting error: $", round(baseline_error, 2), "\n")
cat("Final error: $", round(best_error, 2), "\n")
cat("Total improvement: $", round(baseline_error - best_error, 2), "\n")
cat("Improvement percentage:", round((baseline_error - best_error) / baseline_error * 100, 1), "%\n")

# Display all optimized parameters
cat("\n=== OPTIMIZED CONFIGURATION ===\n")
cat("# Basic calculation parameters\n")
cat("base_per_diem =", best_config$base_per_diem, "\n")
cat("mileage_rates = c(", paste(round(best_config$mileage_rates, 5), collapse = ", "), ")\n")
cat("receipt_rates = c(", paste(round(best_config$receipt_rates, 5), collapse = ", "), ")\n")

cat("\n# Tier boundaries\n")
cat("mileage_tiers = c(", paste(best_config$mileage_tiers, collapse = ", "), ")\n")
cat("receipt_tiers = c(", paste(best_config$receipt_tiers, collapse = ", "), ")\n")

cat("\n# Threshold values\n")
cat("receipt_threshold =", best_config$receipt_threshold, "\n")
cat("efficiency_threshold =", best_config$efficiency_threshold, "\n")
cat("miles_threshold =", best_config$miles_threshold, "\n")

cat("\n# Multipliers\n")
cat("cluster_multipliers = c(", paste(round(best_config$cluster_multipliers, 5), collapse = ", "), ")\n")
cat("five_day_multiplier =", round(best_config$five_day_multiplier, 5), "\n")
cat("receipt_threshold_multiplier =", round(best_config$receipt_threshold_multiplier, 5), "\n")
cat("efficiency_threshold_multiplier =", round(best_config$efficiency_threshold_multiplier, 5), "\n")
cat("miles_threshold_multiplier =", round(best_config$miles_threshold_multiplier, 5), "\n")

cat("\n# Advanced features\n")
cat("interaction_coeff =", best_config$interaction_coeff, "\n")
cat("log_bonus_coeff =", best_config$log_bonus_coeff, "\n")
cat("bug_base_bonus =", best_config$bug_base_bonus, "\n")
cat("bug_multiplier =", best_config$bug_multiplier, "\n")

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
cat("    # Advanced features\n")
cat("    interaction_coeff =", best_config$interaction_coeff, ",\n")
cat("    log_bonus_coeff =", best_config$log_bonus_coeff, ",\n")
cat("    bug_base_bonus =", best_config$bug_base_bonus, ",\n")
cat("    bug_multiplier =", best_config$bug_multiplier, ",\n")
cat("    \n")
cat("    # Rounding bug parameters\n")
cat("    bug_cents_values = c(", paste(best_config$bug_cents_values, collapse = ", "), ")\n")
cat("  ))\n")
cat("}\n")

cat("\n=== ULTIMATE OPTIMIZATION COMPLETE ===\n")
cat("Copy the updated get_config function above into reimbursement_functions.R\n")
cat("Then run 'Rscript eval.R' to test the improved performance!\n")
