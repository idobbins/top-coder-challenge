#!/usr/bin/env Rscript

# Advanced Parameter Optimization for Reimbursement System
# Uses genetic algorithm and other optimization techniques to find optimal parameters

# Load required libraries
suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(parallel)
  library(GA)  # Genetic Algorithm package
})

# Source the calculation functions
source("reimbursement_functions.R")

# Global variables for optimization
GLOBAL_TEST_DATA <- NULL
GLOBAL_BEST_SCORE <- Inf
GLOBAL_BEST_CONFIG <- NULL
GLOBAL_ITERATION <- 0

# Load test data
load_test_data <- function() {
  cat("Loading test data...\n")
  test_data <- jsonlite::fromJSON("public_cases.json")
  data.frame(
    trip_duration_days = test_data$input$trip_duration_days,
    miles_traveled = test_data$input$miles_traveled,
    total_receipts_amount = test_data$input$total_receipts_amount,
    expected_output = test_data$expected_output
  )
}

# Define parameter bounds for optimization
get_parameter_bounds <- function() {
  list(
    # Basic calculation parameters
    base_per_diem = c(30, 120),
    mileage_rate_1 = c(0.3, 1.0),
    mileage_rate_2 = c(0.3, 1.0),
    mileage_rate_3 = c(0.1, 0.8),
    receipt_rate_1 = c(0.2, 1.0),
    receipt_rate_2 = c(0.2, 1.0),
    receipt_rate_3 = c(0.2, 1.0),
    receipt_rate_4 = c(0.05, 0.5),
    
    # Tier boundaries
    mileage_tier_1 = c(50, 150),
    mileage_tier_2 = c(200, 600),
    receipt_tier_1 = c(20, 100),
    receipt_tier_2 = c(200, 800),
    receipt_tier_3 = c(800, 2000),
    
    # Threshold values
    receipt_threshold = c(300, 1200),
    efficiency_threshold = c(100, 300),
    miles_threshold = c(200, 800),
    
    # Cluster multipliers
    cluster_mult_1 = c(0.8, 1.2),
    cluster_mult_2 = c(0.8, 1.2),
    cluster_mult_3 = c(0.8, 1.2),
    cluster_mult_4 = c(0.8, 1.2),
    cluster_mult_5 = c(0.8, 1.2),
    cluster_mult_6 = c(0.8, 1.2),
    
    # Special multipliers
    vacation_bonus_multiplier = c(0.8, 1.2),
    vacation_base_multiplier = c(0.8, 1.2),
    five_day_multiplier = c(0.9, 1.3),
    receipt_threshold_multiplier = c(0.8, 1.2),
    efficiency_threshold_multiplier = c(0.8, 1.2),
    miles_threshold_multiplier = c(0.8, 1.2),
    
    # Advanced features
    interaction_coeff = c(-0.01, 0.01),
    log_bonus_coeff = c(0, 20),
    bug_base_bonus = c(0, 15),
    bug_multiplier = c(0, 0.01)
  )
}

# Convert parameter vector to config object
vector_to_config <- function(param_vector) {
  bounds <- get_parameter_bounds()
  param_names <- names(bounds)
  
  config <- list(
    # Basic calculation parameters
    base_per_diem = param_vector[1],
    mileage_rates = c(param_vector[2], param_vector[3], param_vector[4]),
    receipt_rates = c(param_vector[5], param_vector[6], param_vector[7], param_vector[8]),
    
    # Tier boundaries
    mileage_tiers = c(param_vector[9], param_vector[10]),
    receipt_tiers = c(param_vector[11], param_vector[12], param_vector[13]),
    
    # Threshold values
    receipt_threshold = param_vector[14],
    efficiency_threshold = param_vector[15],
    miles_threshold = param_vector[16],
    
    # Feature engineering thresholds (keep current values)
    efficiency_zones = c(100, 180, 220, 300),
    spending_categories = c(75, 120),
    sweet_spot_duration = c(4, 6),
    
    # Cluster definition parameters (keep current values)
    cluster_boundaries = list(
      very_short_high_eff = c(1.5, 500),
      long_low_eff = c(8, 50),
      medium_high_spend = c(3, 5, 1500),
      medium_high_eff = c(4, 7, 150, 800),
      long_high_spend = c(8, 1200)
    ),
    
    # Multipliers and adjustments
    cluster_multipliers = c(param_vector[17], param_vector[18], param_vector[19], 
                           param_vector[20], param_vector[21], param_vector[22]),
    vacation_bonus_multiplier = param_vector[23],
    vacation_base_multiplier = param_vector[24],
    five_day_multiplier = param_vector[25],
    receipt_threshold_multiplier = param_vector[26],
    efficiency_threshold_multiplier = param_vector[27],
    miles_threshold_multiplier = param_vector[28],
    
    # Advanced features
    interaction_coeff = param_vector[29],
    log_bonus_coeff = param_vector[30],
    bug_base_bonus = param_vector[31],
    bug_multiplier = param_vector[32],
    
    # Enhanced rounding bug parameters (keep current values)
    bug_cents_values = c(49, 99, 1, 51)
  )
  
  return(config)
}

# Fitness function for optimization
fitness_function <- function(param_vector) {
  tryCatch({
    # Convert parameter vector to config
    config <- vector_to_config(param_vector)
    
    # Validate config (ensure logical constraints)
    if (config$mileage_tiers[1] >= config$mileage_tiers[2]) return(-1000)
    if (config$receipt_tiers[1] >= config$receipt_tiers[2] || 
        config$receipt_tiers[2] >= config$receipt_tiers[3]) return(-1000)
    
    # Calculate predictions for all test cases
    predictions <- numeric(nrow(GLOBAL_TEST_DATA))
    
    for (i in 1:nrow(GLOBAL_TEST_DATA)) {
      prediction <- calculate_reimbursement(
        GLOBAL_TEST_DATA$trip_duration_days[i],
        GLOBAL_TEST_DATA$miles_traveled[i],
        GLOBAL_TEST_DATA$total_receipts_amount[i],
        config
      )
      predictions[i] <- prediction
    }
    
    # Calculate errors and metrics
    errors <- abs(predictions - GLOBAL_TEST_DATA$expected_output)
    exact_matches <- sum(errors <= 0.01)
    avg_error <- mean(errors)
    
    # Combined fitness score (lower is better, so we negate for GA maximization)
    # Heavily weight exact matches and average error
    fitness_score <- -(avg_error * 100 + (1000 - exact_matches) * 0.1)
    
    # Track global best
    current_score <- -fitness_score
    if (current_score < GLOBAL_BEST_SCORE) {
      GLOBAL_BEST_SCORE <<- current_score
      GLOBAL_BEST_CONFIG <<- config
      
      cat(sprintf("New best score: %.2f (Exact: %d, Avg Error: $%.2f)\n", 
                  current_score, exact_matches, avg_error))
    }
    
    GLOBAL_ITERATION <<- GLOBAL_ITERATION + 1
    if (GLOBAL_ITERATION %% 50 == 0) {
      cat(sprintf("Iteration %d, Current best: %.2f\n", GLOBAL_ITERATION, GLOBAL_BEST_SCORE))
    }
    
    return(fitness_score)
    
  }, error = function(e) {
    return(-1000)  # Return very poor fitness for invalid configurations
  })
}

# Detailed evaluation function
detailed_evaluation <- function(config) {
  predictions <- numeric(nrow(GLOBAL_TEST_DATA))
  
  for (i in 1:nrow(GLOBAL_TEST_DATA)) {
    prediction <- calculate_reimbursement(
      GLOBAL_TEST_DATA$trip_duration_days[i],
      GLOBAL_TEST_DATA$miles_traveled[i],
      GLOBAL_TEST_DATA$total_receipts_amount[i],
      config
    )
    predictions[i] <- prediction
  }
  
  errors <- abs(predictions - GLOBAL_TEST_DATA$expected_output)
  exact_matches <- sum(errors <= 0.01)
  close_matches <- sum(errors <= 1.00)
  avg_error <- mean(errors)
  median_error <- median(errors)
  max_error <- max(errors)
  rmse <- sqrt(mean(errors^2))
  score <- avg_error * 100 + (1000 - exact_matches) * 0.1
  
  list(
    exact_matches = exact_matches,
    close_matches = close_matches,
    avg_error = avg_error,
    median_error = median_error,
    max_error = max_error,
    rmse = rmse,
    score = score,
    predictions = predictions,
    errors = errors
  )
}

# Multi-strategy optimization
run_optimization <- function(strategy = "genetic", max_iterations = 1000) {
  cat("Starting", strategy, "optimization...\n")
  
  bounds <- get_parameter_bounds()
  lower_bounds <- sapply(bounds, function(x) x[1])
  upper_bounds <- sapply(bounds, function(x) x[2])
  
  if (strategy == "genetic") {
    # Genetic Algorithm optimization
    result <- ga(
      type = "real-valued",
      fitness = fitness_function,
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 50,
      maxiter = max_iterations,
      run = 50,  # Stop if no improvement for 50 generations
      parallel = TRUE,
      monitor = TRUE,
      seed = 123
    )
    
    return(list(
      best_params = result@solution[1, ],
      best_fitness = result@fitnessValue,
      convergence = result@iter
    ))
    
  } else if (strategy == "random_search") {
    # Random search for comparison
    best_fitness <- -Inf
    best_params <- NULL
    
    for (i in 1:max_iterations) {
      # Generate random parameters within bounds
      params <- numeric(length(bounds))
      for (j in 1:length(bounds)) {
        params[j] <- runif(1, lower_bounds[j], upper_bounds[j])
      }
      
      fitness <- fitness_function(params)
      
      if (fitness > best_fitness) {
        best_fitness <- fitness
        best_params <- params
      }
      
      if (i %% 100 == 0) {
        cat("Random search iteration", i, "best fitness:", best_fitness, "\n")
      }
    }
    
    return(list(
      best_params = best_params,
      best_fitness = best_fitness,
      convergence = max_iterations
    ))
  }
}

# Save optimized configuration
save_optimized_config <- function(config, filename = "optimized_config.R") {
  cat("Saving optimized configuration to", filename, "...\n")
  
  config_code <- paste0(
    "# Optimized Configuration Generated by Advanced Parameter Optimizer\n",
    "# Generated on: ", Sys.time(), "\n\n",
    "get_optimized_config <- function() {\n",
    "  return(list(\n",
    "    # Basic calculation parameters\n",
    "    base_per_diem = ", config$base_per_diem, ",\n",
    "    mileage_rates = c(", paste(config$mileage_rates, collapse = ", "), "),\n",
    "    receipt_rates = c(", paste(config$receipt_rates, collapse = ", "), "),\n",
    "    \n",
    "    # Tier boundaries\n",
    "    mileage_tiers = c(", paste(config$mileage_tiers, collapse = ", "), "),\n",
    "    receipt_tiers = c(", paste(config$receipt_tiers, collapse = ", "), "),\n",
    "    \n",
    "    # Threshold values\n",
    "    receipt_threshold = ", config$receipt_threshold, ",\n",
    "    efficiency_threshold = ", config$efficiency_threshold, ",\n",
    "    miles_threshold = ", config$miles_threshold, ",\n",
    "    \n",
    "    # Feature engineering thresholds\n",
    "    efficiency_zones = c(", paste(config$efficiency_zones, collapse = ", "), "),\n",
    "    spending_categories = c(", paste(config$spending_categories, collapse = ", "), "),\n",
    "    sweet_spot_duration = c(", paste(config$sweet_spot_duration, collapse = ", "), "),\n",
    "    \n",
    "    # Cluster definition parameters\n",
    "    cluster_boundaries = list(\n",
    "      very_short_high_eff = c(", paste(config$cluster_boundaries$very_short_high_eff, collapse = ", "), "),\n",
    "      long_low_eff = c(", paste(config$cluster_boundaries$long_low_eff, collapse = ", "), "),\n",
    "      medium_high_spend = c(", paste(config$cluster_boundaries$medium_high_spend, collapse = ", "), "),\n",
    "      medium_high_eff = c(", paste(config$cluster_boundaries$medium_high_eff, collapse = ", "), "),\n",
    "      long_high_spend = c(", paste(config$cluster_boundaries$long_high_spend, collapse = ", "), ")\n",
    "    ),\n",
    "    \n",
    "    # Multipliers and adjustments\n",
    "    cluster_multipliers = c(", paste(config$cluster_multipliers, collapse = ", "), "),\n",
    "    vacation_bonus_multiplier = ", config$vacation_bonus_multiplier, ",\n",
    "    vacation_base_multiplier = ", config$vacation_base_multiplier, ",\n",
    "    five_day_multiplier = ", config$five_day_multiplier, ",\n",
    "    receipt_threshold_multiplier = ", config$receipt_threshold_multiplier, ",\n",
    "    efficiency_threshold_multiplier = ", config$efficiency_threshold_multiplier, ",\n",
    "    miles_threshold_multiplier = ", config$miles_threshold_multiplier, ",\n",
    "    \n",
    "    # Advanced features\n",
    "    interaction_coeff = ", config$interaction_coeff, ",\n",
    "    log_bonus_coeff = ", config$log_bonus_coeff, ",\n",
    "    bug_base_bonus = ", config$bug_base_bonus, ",\n",
    "    bug_multiplier = ", config$bug_multiplier, ",\n",
    "    \n",
    "    # Enhanced rounding bug parameters\n",
    "    bug_cents_values = c(", paste(config$bug_cents_values, collapse = ", "), ")\n",
    "  ))\n",
    "}\n"
  )
  
  writeLines(config_code, filename)
}

# Main optimization function
main_optimization <- function(strategies = c("genetic"), max_iterations = 500) {
  cat("=== ADVANCED PARAMETER OPTIMIZATION ===\n")
  cat("Loading test data and initializing...\n")
  
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
  
  # Run optimization strategies
  best_overall_config <- baseline_config
  best_overall_score <- baseline_results$score
  
  for (strategy in strategies) {
    cat("Running", strategy, "optimization...\n")
    
    # Reset global tracking
    GLOBAL_BEST_SCORE <<- Inf
    GLOBAL_BEST_CONFIG <<- NULL
    GLOBAL_ITERATION <<- 0
    
    # Run optimization
    result <- run_optimization(strategy, max_iterations)
    
    if (!is.null(GLOBAL_BEST_CONFIG) && GLOBAL_BEST_SCORE < best_overall_score) {
      best_overall_config <- GLOBAL_BEST_CONFIG
      best_overall_score <- GLOBAL_BEST_SCORE
    }
    
    cat("Completed", strategy, "optimization\n")
    cat("Best score achieved:", GLOBAL_BEST_SCORE, "\n\n")
  }
  
  # Final evaluation of best configuration
  cat("=== FINAL OPTIMIZATION RESULTS ===\n")
  final_results <- detailed_evaluation(best_overall_config)
  
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
  save_optimized_config(best_overall_config)
  
  cat("Optimization complete! Check 'optimized_config.R' for the new configuration.\n")
  cat("To use the optimized config, replace get_config() with get_optimized_config() in your code.\n")
  
  return(list(
    baseline_results = baseline_results,
    optimized_results = final_results,
    optimized_config = best_overall_config
  ))
}

# Run the optimization if script is executed directly
if (!interactive()) {
  # Check if GA package is available
  if (!requireNamespace("GA", quietly = TRUE)) {
    cat("Installing GA package for genetic algorithm optimization...\n")
    install.packages("GA", repos = "https://cran.r-project.org")
    library(GA)
  }
  
  # Run optimization with genetic algorithm
  results <- main_optimization(strategies = c("genetic"), max_iterations = 200)
}
