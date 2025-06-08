#!/usr/bin/env Rscript

# Ultimate Parameter Optimization - Comprehensive Hyperparameter Tuning
# Combines threshold analysis, tier optimization, and advanced parameter tuning
# WITH CONVERGENCE CRITERIA TO PREVENT INFINITE LOOPS

library(dplyr)
source("reimbursement_functions.R")

# =============================================================================
# CONVERGENCE CONFIGURATION
# =============================================================================
convergence_config <- list(
  min_improvement = 0.10,           # Minimum improvement in dollars to continue
  min_relative_improvement = 0.001, # Minimum relative improvement (0.1%)
  max_iterations_per_phase = 1000,  # Maximum iterations per optimization phase
  max_total_runtime = 3600,         # Maximum total runtime in seconds (1 hour)
  patience = 50,                    # Stop if no improvement for this many iterations
  coarse_search_factor = 3,         # Factor for coarse search step reduction
  fine_search_threshold = 5.0       # Switch to fine search when error < this
)

# Global tracking variables
optimization_start_time <- Sys.time()
global_iteration_count <- 0
phase_iteration_counts <- list()

# Helper function to check convergence
check_convergence <- function(current_error, best_error, iteration_count, 
                             phase_name, no_improvement_count = 0) {
  
  # Check runtime limit
  elapsed_time <- as.numeric(difftime(Sys.time(), optimization_start_time, units = "secs"))
  if (elapsed_time > convergence_config$max_total_runtime) {
    cat("  STOPPING: Maximum runtime (", convergence_config$max_total_runtime, "s) exceeded\n")
    return(TRUE)
  }
  
  # Check iteration limit
  if (iteration_count > convergence_config$max_iterations_per_phase) {
    cat("  STOPPING: Maximum iterations (", convergence_config$max_iterations_per_phase, ") for phase", phase_name, "\n")
    return(TRUE)
  }
  
  # Check patience (no improvement)
  if (no_improvement_count > convergence_config$patience) {
    cat("  STOPPING: No improvement for", convergence_config$patience, "iterations in phase", phase_name, "\n")
    return(TRUE)
  }
  
  # Check if improvement is significant
  improvement <- best_error - current_error
  relative_improvement <- improvement / best_error
  
  if (improvement < convergence_config$min_improvement && 
      relative_improvement < convergence_config$min_relative_improvement) {
    if (iteration_count > 10) {  # Allow at least 10 iterations before checking
      cat("  STOPPING: Improvement too small (", round(improvement, 4), ") in phase", phase_name, "\n")
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# Helper function to get adaptive step size
get_adaptive_step <- function(current_error, base_step, phase_name) {
  if (current_error < convergence_config$fine_search_threshold) {
    return(base_step / convergence_config$coarse_search_factor)
  }
  return(base_step)
}

# Load training data
cat("=== ULTIMATE OPTIMIZATION STARTING ===\n")
cat("Convergence settings:\n")
cat("  Min improvement: $", convergence_config$min_improvement, "\n")
cat("  Min relative improvement: ", convergence_config$min_relative_improvement * 100, "%\n")
cat("  Max runtime: ", convergence_config$max_total_runtime, " seconds\n")
cat("  Patience: ", convergence_config$patience, " iterations\n")
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
phase_start_error <- best_error
iteration_count <- 0
no_improvement_count <- 0

# Optimize receipt threshold
cat("Optimizing receipt threshold...\n")
base_step <- 25
step_size <- get_adaptive_step(best_error, base_step, "Phase1")
receipt_thresholds <- seq(500, 800, step_size)
for (thresh in receipt_thresholds) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(receipt_threshold = thresh))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$receipt_threshold <- thresh
    no_improvement_count <- 0
    cat("  New best receipt threshold:", thresh, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase1-Receipt", no_improvement_count)) {
    break
  }
}

# Optimize efficiency threshold
cat("Optimizing efficiency threshold...\n")
step_size <- get_adaptive_step(best_error, 10, "Phase1")
efficiency_thresholds <- seq(150, 220, step_size)
for (thresh in efficiency_thresholds) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(efficiency_threshold = thresh))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$efficiency_threshold <- thresh
    no_improvement_count <- 0
    cat("  New best efficiency threshold:", thresh, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase1-Efficiency", no_improvement_count)) {
    break
  }
}

# Optimize miles threshold
cat("Optimizing miles threshold...\n")
step_size <- get_adaptive_step(best_error, 25, "Phase1")
miles_thresholds <- seq(400, 600, step_size)
for (thresh in miles_thresholds) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(miles_threshold = thresh))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$miles_threshold <- thresh
    no_improvement_count <- 0
    cat("  New best miles threshold:", thresh, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase1-Miles", no_improvement_count)) {
    break
  }
}

phase_improvement <- phase_start_error - best_error
cat("Phase 1 complete. Error reduced to: $", round(best_error, 2), 
    " (improvement: $", round(phase_improvement, 4), ")\n")

# =============================================================================
# PHASE 2: TIER BOUNDARY OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 2: TIER BOUNDARY OPTIMIZATION ===\n")
phase_start_error <- best_error
iteration_count <- 0
no_improvement_count <- 0

# Optimize mileage tiers
cat("Optimizing mileage tier boundaries...\n")
step1 <- get_adaptive_step(best_error, 10, "Phase2")
step2 <- get_adaptive_step(best_error, 25, "Phase2")
for (tier1 in seq(80, 120, step1)) {
  for (tier2 in seq(450, 550, step2)) {
    if (tier2 > tier1) {
      iteration_count <- iteration_count + 1
      error <- objective_function(list(mileage_tiers = c(tier1, tier2)))
      
      if (error < best_error) {
        improvement <- best_error - error
        best_error <- error
        best_config$mileage_tiers <- c(tier1, tier2)
        no_improvement_count <- 0
        cat("  New best mileage tiers:", tier1, tier2, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
      } else {
        no_improvement_count <- no_improvement_count + 1
      }
      
      if (check_convergence(error, best_error, iteration_count, "Phase2-Mileage", no_improvement_count)) {
        break
      }
    }
  }
  if (check_convergence(error, best_error, iteration_count, "Phase2-Mileage", no_improvement_count)) {
    break
  }
}

# Optimize receipt tiers (with reduced search space for efficiency)
cat("Optimizing receipt tier boundaries...\n")
step1 <- max(5, get_adaptive_step(best_error, 5, "Phase2"))
step2 <- max(25, get_adaptive_step(best_error, 25, "Phase2"))
step3 <- max(50, get_adaptive_step(best_error, 50, "Phase2"))

tier_search_complete <- FALSE
for (tier1 in seq(40, 60, step1)) {
  for (tier2 in seq(450, 550, step2)) {
    for (tier3 in seq(1400, 1600, step3)) {
      if (tier2 > tier1 && tier3 > tier2) {
        iteration_count <- iteration_count + 1
        error <- objective_function(list(receipt_tiers = c(tier1, tier2, tier3)))
        
        if (error < best_error) {
          improvement <- best_error - error
          best_error <- error
          best_config$receipt_tiers <- c(tier1, tier2, tier3)
          no_improvement_count <- 0
          cat("  New best receipt tiers:", tier1, tier2, tier3, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
        } else {
          no_improvement_count <- no_improvement_count + 1
        }
        
        if (check_convergence(error, best_error, iteration_count, "Phase2-Receipt", no_improvement_count)) {
          tier_search_complete <- TRUE
          break
        }
      }
    }
    if (tier_search_complete) break
  }
  if (tier_search_complete) break
}

phase_improvement <- phase_start_error - best_error
cat("Phase 2 complete. Error reduced to: $", round(best_error, 2), 
    " (improvement: $", round(phase_improvement, 4), ")\n")

# =============================================================================
# PHASE 3: RATE OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 3: RATE OPTIMIZATION ===\n")
phase_start_error <- best_error
iteration_count <- 0
no_improvement_count <- 0

# Optimize base per diem
cat("Optimizing base per diem...\n")
step_size <- get_adaptive_step(best_error, 2, "Phase3")
for (per_diem in seq(best_config$base_per_diem - 10, best_config$base_per_diem + 10, step_size)) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(base_per_diem = per_diem))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$base_per_diem <- per_diem
    no_improvement_count <- 0
    cat("  New best per diem:", per_diem, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase3-PerDiem", no_improvement_count)) {
    break
  }
}

# Optimize mileage rates (with early stopping)
cat("Optimizing mileage rates...\n")
current_rates <- best_config$mileage_rates
rate_step <- get_adaptive_step(best_error, 0.02, "Phase3")
rate_search_complete <- FALSE

for (rate1 in seq(current_rates[1] - 0.1, current_rates[1] + 0.1, rate_step)) {
  for (rate2 in seq(current_rates[2] - 0.1, current_rates[2] + 0.1, rate_step)) {
    for (rate3 in seq(current_rates[3] - 0.1, current_rates[3] + 0.1, rate_step)) {
      iteration_count <- iteration_count + 1
      error <- objective_function(list(mileage_rates = c(rate1, rate2, rate3)))
      
      if (error < best_error) {
        improvement <- best_error - error
        best_error <- error
        best_config$mileage_rates <- c(rate1, rate2, rate3)
        no_improvement_count <- 0
        cat("  New best mileage rates:", round(rate1, 3), round(rate2, 3), round(rate3, 3), "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
      } else {
        no_improvement_count <- no_improvement_count + 1
      }
      
      if (check_convergence(error, best_error, iteration_count, "Phase3-Mileage", no_improvement_count)) {
        rate_search_complete <- TRUE
        break
      }
    }
    if (rate_search_complete) break
  }
  if (rate_search_complete) break
}

# Optimize receipt rates (with early stopping and reduced search space)
cat("Optimizing receipt rates...\n")
current_rates <- best_config$receipt_rates
receipt_step1 <- get_adaptive_step(best_error, 0.02, "Phase3")
receipt_step2 <- get_adaptive_step(best_error, 0.01, "Phase3")
receipt_search_complete <- FALSE

for (rate1 in seq(current_rates[1] - 0.1, current_rates[1] + 0.1, receipt_step1)) {
  for (rate2 in seq(current_rates[2] - 0.1, current_rates[2] + 0.1, receipt_step1)) {
    for (rate3 in seq(current_rates[3] - 0.1, current_rates[3] + 0.1, receipt_step1)) {
      for (rate4 in seq(current_rates[4] - 0.05, current_rates[4] + 0.05, receipt_step2)) {
        iteration_count <- iteration_count + 1
        error <- objective_function(list(receipt_rates = c(rate1, rate2, rate3, rate4)))
        
        if (error < best_error) {
          improvement <- best_error - error
          best_error <- error
          best_config$receipt_rates <- c(rate1, rate2, rate3, rate4)
          no_improvement_count <- 0
          cat("  New best receipt rates:", round(rate1, 3), round(rate2, 3), round(rate3, 3), round(rate4, 3), "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
        } else {
          no_improvement_count <- no_improvement_count + 1
        }
        
        if (check_convergence(error, best_error, iteration_count, "Phase3-Receipt", no_improvement_count)) {
          receipt_search_complete <- TRUE
          break
        }
      }
      if (receipt_search_complete) break
    }
    if (receipt_search_complete) break
  }
  if (receipt_search_complete) break
}

phase_improvement <- phase_start_error - best_error
cat("Phase 3 complete. Error reduced to: $", round(best_error, 2), 
    " (improvement: $", round(phase_improvement, 4), ")\n")

# =============================================================================
# PHASE 4: MULTIPLIER OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 4: MULTIPLIER OPTIMIZATION ===\n")
phase_start_error <- best_error
iteration_count <- 0
no_improvement_count <- 0

# Optimize cluster multipliers
cat("Optimizing cluster multipliers...\n")
mult_step <- get_adaptive_step(best_error, 0.01, "Phase4")
current_mults <- best_config$cluster_multipliers
for (i in 1:length(current_mults)) {
  for (mult in seq(current_mults[i] - 0.05, current_mults[i] + 0.05, mult_step)) {
    iteration_count <- iteration_count + 1
    new_mults <- current_mults
    new_mults[i] <- mult
    error <- objective_function(list(cluster_multipliers = new_mults))
    
    if (error < best_error) {
      improvement <- best_error - error
      best_error <- error
      best_config$cluster_multipliers <- new_mults
      no_improvement_count <- 0
      cat("  New best cluster", i-1, "multiplier:", round(mult, 3), "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
    } else {
      no_improvement_count <- no_improvement_count + 1
    }
    
    if (check_convergence(error, best_error, iteration_count, "Phase4-Cluster", no_improvement_count)) {
      break
    }
  }
  if (check_convergence(error, best_error, iteration_count, "Phase4-Cluster", no_improvement_count)) {
    break
  }
}

# Optimize threshold multipliers
cat("Optimizing threshold multipliers...\n")
threshold_multipliers <- c("five_day_multiplier", "receipt_threshold_multiplier", 
                          "efficiency_threshold_multiplier", "miles_threshold_multiplier")
thresh_step <- get_adaptive_step(best_error, 0.02, "Phase4")

for (mult_name in threshold_multipliers) {
  current_val <- best_config[[mult_name]]
  for (mult in seq(current_val - 0.1, current_val + 0.1, thresh_step)) {
    iteration_count <- iteration_count + 1
    update <- list()
    update[[mult_name]] <- mult
    error <- objective_function(update)
    
    if (error < best_error) {
      improvement <- best_error - error
      best_error <- error
      best_config[[mult_name]] <- mult
      no_improvement_count <- 0
      cat("  New best", mult_name, ":", round(mult, 3), "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
    } else {
      no_improvement_count <- no_improvement_count + 1
    }
    
    if (check_convergence(error, best_error, iteration_count, "Phase4-Threshold", no_improvement_count)) {
      break
    }
  }
  if (check_convergence(error, best_error, iteration_count, "Phase4-Threshold", no_improvement_count)) {
    break
  }
}

phase_improvement <- phase_start_error - best_error
cat("Phase 4 complete. Error reduced to: $", round(best_error, 2), 
    " (improvement: $", round(phase_improvement, 4), ")\n")

# =============================================================================
# PHASE 5: ADVANCED FEATURE OPTIMIZATION
# =============================================================================
cat("\n=== PHASE 5: ADVANCED FEATURE OPTIMIZATION ===\n")
phase_start_error <- best_error
iteration_count <- 0
no_improvement_count <- 0

# Optimize interaction coefficient
cat("Optimizing interaction coefficient...\n")
coeff_step <- get_adaptive_step(best_error, 0.0002, "Phase5")
for (coeff in seq(-0.002, 0.002, coeff_step)) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(interaction_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$interaction_coeff <- coeff
    no_improvement_count <- 0
    cat("  New best interaction coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase5-Interaction", no_improvement_count)) {
    break
  }
}

# Optimize log bonus coefficient
cat("Optimizing log bonus coefficient...\n")
log_step <- get_adaptive_step(best_error, 1, "Phase5")
for (coeff in seq(-10, 10, log_step)) {
  iteration_count <- iteration_count + 1
  error <- objective_function(list(log_bonus_coeff = coeff))
  
  if (error < best_error) {
    improvement <- best_error - error
    best_error <- error
    best_config$log_bonus_coeff <- coeff
    no_improvement_count <- 0
    cat("  New best log bonus coeff:", coeff, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
  } else {
    no_improvement_count <- no_improvement_count + 1
  }
  
  if (check_convergence(error, best_error, iteration_count, "Phase5-LogBonus", no_improvement_count)) {
    break
  }
}

# Optimize bug parameters (with early stopping)
cat("Optimizing bug parameters...\n")
bug_step1 <- get_adaptive_step(best_error, 5, "Phase5")
bug_step2 <- get_adaptive_step(best_error, 0.002, "Phase5")
bug_search_complete <- FALSE

for (base_bonus in seq(0, 50, bug_step1)) {
  for (multiplier in seq(0, 0.02, bug_step2)) {
    iteration_count <- iteration_count + 1
    error <- objective_function(list(bug_base_bonus = base_bonus, bug_multiplier = multiplier))
    
    if (error < best_error) {
      improvement <- best_error - error
      best_error <- error
      best_config$bug_base_bonus <- base_bonus
      best_config$bug_multiplier <- multiplier
      no_improvement_count <- 0
      cat("  New best bug params:", base_bonus, multiplier, "error:", round(error, 2), "improvement:", round(improvement, 4), "\n")
    } else {
      no_improvement_count <- no_improvement_count + 1
    }
    
    if (check_convergence(error, best_error, iteration_count, "Phase5-Bug", no_improvement_count)) {
      bug_search_complete <- TRUE
      break
    }
  }
  if (bug_search_complete) break
}

phase_improvement <- phase_start_error - best_error
cat("Phase 5 complete. Error reduced to: $", round(best_error, 2), 
    " (improvement: $", round(phase_improvement, 4), ")\n")

# Final runtime summary
total_elapsed_time <- as.numeric(difftime(Sys.time(), optimization_start_time, units = "secs"))
cat("Total optimization time:", round(total_elapsed_time, 1), "seconds\n")

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
