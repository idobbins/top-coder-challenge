# Advanced Parameter Optimization for Legacy Reimbursement System
# Phase 1: R-based optimization with sophisticated algorithms

# Load required libraries
library(jsonlite)
library(dplyr)
library(DEoptim)
library(parallel)
library(ggplot2)
library(corrplot)

# Set up parallel processing
num_cores <- detectCores() - 1
cat("Using", num_cores, "cores for parallel processing\n")

# Load training data
cat("Loading training data...\n")
public_cases <- read.csv("public_cases.csv")

# Extract inputs and targets
inputs <- data.frame(
  trip_duration_days = public_cases$trip_duration_days,
  miles_traveled = public_cases$miles_traveled,
  total_receipts_amount = public_cases$total_receipts_amount
)
targets <- public_cases$expected_output

cat("Loaded", nrow(inputs), "training cases\n")

# Feature engineering function (R implementation of Python logic)
engineer_features <- function(trip_duration_days, miles_traveled, total_receipts_amount) {
  # Basic rate features
  receipts_per_day <- total_receipts_amount / trip_duration_days
  receipts_per_mile <- ifelse(miles_traveled > 0, total_receipts_amount / miles_traveled, 0)
  miles_per_day <- miles_traveled / trip_duration_days
  
  # Efficiency-based features
  efficiency_category <- ifelse(miles_per_day < 100, 0,
                               ifelse(miles_per_day < 180, 1,
                                     ifelse(miles_per_day <= 220, 2,
                                           ifelse(miles_per_day <= 300, 3, 4))))
  
  efficiency_bonus_zone <- ifelse(miles_per_day >= 180 & miles_per_day <= 220, 1, 0)
  efficiency_penalty_zone <- ifelse(miles_per_day > 300, 1, 0)
  
  # Spending pattern features
  spending_category <- ifelse(receipts_per_day < 75, 0,
                             ifelse(receipts_per_day <= 120, 1, 2))
  
  very_low_receipts <- ifelse(total_receipts_amount < 50, 1, 0)
  optimal_receipt_range <- ifelse(total_receipts_amount >= 600 & total_receipts_amount <= 800, 1, 0)
  high_spending_flag <- ifelse(total_receipts_amount > 1000, 1, 0)
  
  # CRITICAL: The rounding bug
  receipt_cents <- (total_receipts_amount * 100) %% 100
  receipt_ends_49_99 <- ifelse(receipt_cents %in% c(49, 99), 1, 0)
  
  # Trip length features
  is_5_day_trip <- ifelse(trip_duration_days == 5, 1, 0)
  is_sweet_spot_duration <- ifelse(trip_duration_days >= 4 & trip_duration_days <= 6, 1, 0)
  duration_penalty_zone <- ifelse(trip_duration_days < 2 | trip_duration_days > 10, 1, 0)
  very_short_trip <- ifelse(trip_duration_days == 1, 1, 0)
  very_long_trip <- ifelse(trip_duration_days >= 8, 1, 0)
  
  # Combination features
  sweet_spot_combo <- ifelse(trip_duration_days == 5 & miles_per_day >= 180 & receipts_per_day < 100, 1, 0)
  vacation_penalty <- ifelse(trip_duration_days >= 8 & receipts_per_day > 120, 1, 0)
  high_mile_low_spend <- ifelse(miles_per_day > 200 & receipts_per_day < 80, 1, 0)
  low_mile_high_spend <- ifelse(miles_per_day < 100 & receipts_per_day > 100, 1, 0)
  
  # Interaction features
  efficiency_spending_interaction <- miles_per_day * receipts_per_day
  duration_efficiency_interaction <- trip_duration_days * miles_per_day
  duration_spending_interaction <- trip_duration_days * receipts_per_day
  
  # Threshold features
  short_high_efficiency <- ifelse(trip_duration_days <= 3 & miles_per_day > 150, 1, 0)
  long_low_efficiency <- ifelse(trip_duration_days >= 7 & miles_per_day < 100, 1, 0)
  medium_balanced <- ifelse(trip_duration_days >= 4 & trip_duration_days <= 6 & 
                           miles_per_day >= 100 & miles_per_day <= 200 & 
                           receipts_per_day >= 50 & receipts_per_day <= 150, 1, 0)
  
  # Mathematical transformations
  log_trip_duration <- log(trip_duration_days + 1)
  log_miles_traveled <- log(miles_traveled + 1)
  log_receipts <- log(total_receipts_amount + 1)
  
  trip_duration_squared <- trip_duration_days^2
  miles_traveled_squared <- miles_traveled^2
  receipts_squared <- total_receipts_amount^2
  trip_duration_cubed <- trip_duration_days^3
  
  # Ratio and efficiency features
  total_efficiency <- (miles_traveled * trip_duration_days) / (total_receipts_amount + 1)
  cost_per_mile <- ifelse(miles_traveled > 0, total_receipts_amount / miles_traveled, 0)
  productivity_score <- miles_traveled / (trip_duration_days * (total_receipts_amount + 1))
  
  return(list(
    trip_duration_days = trip_duration_days,
    miles_traveled = miles_traveled,
    total_receipts_amount = total_receipts_amount,
    receipts_per_day = receipts_per_day,
    receipts_per_mile = receipts_per_mile,
    miles_per_day = miles_per_day,
    efficiency_bonus_zone = efficiency_bonus_zone,
    efficiency_penalty_zone = efficiency_penalty_zone,
    very_low_receipts = very_low_receipts,
    optimal_receipt_range = optimal_receipt_range,
    high_spending_flag = high_spending_flag,
    receipt_ends_49_99 = receipt_ends_49_99,
    is_5_day_trip = is_5_day_trip,
    is_sweet_spot_duration = is_sweet_spot_duration,
    duration_penalty_zone = duration_penalty_zone,
    very_short_trip = very_short_trip,
    very_long_trip = very_long_trip,
    sweet_spot_combo = sweet_spot_combo,
    vacation_penalty = vacation_penalty,
    high_mile_low_spend = high_mile_low_spend,
    low_mile_high_spend = low_mile_high_spend,
    efficiency_spending_interaction = efficiency_spending_interaction,
    duration_efficiency_interaction = duration_efficiency_interaction,
    duration_spending_interaction = duration_spending_interaction,
    short_high_efficiency = short_high_efficiency,
    long_low_efficiency = long_low_efficiency,
    medium_balanced = medium_balanced,
    trip_duration_squared = trip_duration_squared,
    miles_traveled_squared = miles_traveled_squared,
    receipts_squared = receipts_squared,
    trip_duration_cubed = trip_duration_cubed,
    log_trip_duration = log_trip_duration,
    log_miles_traveled = log_miles_traveled,
    log_receipts = log_receipts,
    total_efficiency = total_efficiency,
    cost_per_mile = cost_per_mile,
    productivity_score = productivity_score
  ))
}

# Cluster determination function
determine_cluster <- function(features) {
  trip_duration <- features$trip_duration_days
  miles_traveled <- features$miles_traveled
  receipts <- features$total_receipts_amount
  miles_per_day <- features$miles_per_day
  receipts_per_day <- features$receipts_per_day
  
  if (trip_duration <= 1.5 && miles_per_day > 500) {
    return(0)  # Very short, extremely high efficiency
  } else if (trip_duration > 8 && miles_per_day < 50) {
    return(1)  # Long, very low efficiency
  } else if (trip_duration >= 3 && trip_duration <= 5 && receipts > 1500) {
    return(2)  # Medium trips, very high spending
  } else if (trip_duration >= 4 && trip_duration <= 7 && miles_per_day > 150 && receipts < 800) {
    return(3)  # Medium, high efficiency, low spending
  } else if (trip_duration > 8 && receipts > 1200) {
    return(4)  # Long trips, high spending
  } else {
    return(5)  # Everything else
  }
}

# Main reimbursement calculation function
calculate_reimbursement <- function(trip_duration_days, miles_traveled, total_receipts_amount, params) {
  # Extract parameters
  base_per_diem <- params[1]
  mileage_rate_1 <- params[2]
  mileage_rate_2 <- params[3]
  mileage_rate_3 <- params[4]
  receipt_rate_1 <- params[5]
  receipt_rate_2 <- params[6]
  receipt_rate_3 <- params[7]
  receipt_rate_4 <- params[8]
  cluster_mult_0 <- params[9]
  cluster_mult_1 <- params[10]
  cluster_mult_2 <- params[11]
  cluster_mult_3 <- params[12]
  cluster_mult_4 <- params[13]
  cluster_mult_5 <- params[14]
  vacation_bonus_mult <- params[15]
  vacation_base_mult <- params[16]
  five_day_mult <- params[17]
  receipt_threshold_mult <- params[18]
  efficiency_threshold_mult <- params[19]
  miles_threshold_mult <- params[20]
  interaction_coeff <- params[21]
  log_bonus_coeff <- params[22]
  bug_base_bonus <- params[23]
  bug_multiplier <- params[24]
  
  # Engineer features
  features <- engineer_features(trip_duration_days, miles_traveled, total_receipts_amount)
  
  # Determine cluster
  cluster <- determine_cluster(features)
  
  # Calculate base reimbursement
  base_per_diem_amount <- base_per_diem * trip_duration_days
  
  # Mileage calculation
  if (miles_traveled <= 100) {
    mileage_reimbursement <- miles_traveled * mileage_rate_1
  } else if (miles_traveled <= 500) {
    mileage_reimbursement <- 100 * mileage_rate_1 + (miles_traveled - 100) * mileage_rate_2
  } else {
    mileage_reimbursement <- 100 * mileage_rate_1 + 400 * mileage_rate_2 + (miles_traveled - 500) * mileage_rate_3
  }
  
  # Receipt reimbursement
  if (total_receipts_amount <= 50) {
    receipt_reimbursement <- total_receipts_amount * receipt_rate_1
  } else if (total_receipts_amount <= 500) {
    receipt_reimbursement <- 15 + (total_receipts_amount - 50) * receipt_rate_2
  } else if (total_receipts_amount <= 1500) {
    receipt_reimbursement <- 15 + 180 + (total_receipts_amount - 500) * receipt_rate_3
  } else {
    receipt_reimbursement <- 15 + 180 + 300 + (total_receipts_amount - 1500) * receipt_rate_4
  }
  
  base_amount <- base_per_diem_amount + mileage_reimbursement + receipt_reimbursement
  
  # Apply cluster adjustments
  cluster_multipliers <- c(cluster_mult_0, cluster_mult_1, cluster_mult_2, cluster_mult_3, cluster_mult_4, cluster_mult_5)
  
  if (cluster == 4 && features$vacation_penalty == 1) {
    adjusted_amount <- base_amount * vacation_bonus_mult
  } else if (cluster == 4) {
    adjusted_amount <- base_amount * vacation_base_mult
  } else {
    adjusted_amount <- base_amount * cluster_multipliers[cluster + 1]
  }
  
  # Apply threshold effects
  threshold_adjusted <- adjusted_amount
  
  if (features$is_5_day_trip == 1) {
    threshold_adjusted <- threshold_adjusted * five_day_mult
  }
  
  if (total_receipts_amount > 660.54) {
    threshold_adjusted <- threshold_adjusted * receipt_threshold_mult
  }
  
  if (features$miles_per_day > 187.01) {
    threshold_adjusted <- threshold_adjusted * efficiency_threshold_mult
  }
  
  if (miles_traveled > 473.8) {
    threshold_adjusted <- threshold_adjusted * miles_threshold_mult
  }
  
  # Apply rounding bug
  if (features$receipt_ends_49_99 == 1) {
    bug_bonus <- bug_base_bonus + (threshold_adjusted * bug_multiplier)
    final_amount <- threshold_adjusted + bug_bonus
  } else {
    final_amount <- threshold_adjusted
  }
  
  # Apply interaction effects
  interaction_bonus <- features$duration_spending_interaction * interaction_coeff
  log_bonus <- features$log_receipts * log_bonus_coeff
  
  final_amount <- final_amount + interaction_bonus + log_bonus
  
  # Ensure reasonable bounds
  final_amount <- pmax(50.0, pmin(final_amount, 5000.0))
  
  return(round(final_amount, 2))
}

# Objective function for optimization
objective_function <- function(params) {
  errors <- numeric(nrow(inputs))
  
  for (i in 1:nrow(inputs)) {
    tryCatch({
      prediction <- calculate_reimbursement(
        inputs$trip_duration_days[i],
        inputs$miles_traveled[i],
        inputs$total_receipts_amount[i],
        params
      )
      errors[i] <- abs(prediction - targets[i])
    }, error = function(e) {
      errors[i] <- 1000.0  # Large penalty for errors
    })
  }
  
  return(mean(errors))
}

# Current baseline parameters (from Python implementation)
baseline_params <- c(
  60,    # base_per_diem
  0.45,  # mileage_rate_1
  0.35,  # mileage_rate_2
  0.4,   # mileage_rate_3
  0.3,   # receipt_rate_1
  0.4,   # receipt_rate_2
  0.3,   # receipt_rate_3
  0.15,  # receipt_rate_4
  1.05,  # cluster_mult_0
  0.98,  # cluster_mult_1
  1.03,  # cluster_mult_2
  1.1,   # cluster_mult_3
  1.08,  # cluster_mult_4
  0.95,  # cluster_mult_5
  1.08,  # vacation_bonus_mult
  1.02,  # vacation_base_mult
  1.12,  # five_day_mult
  1.05,  # receipt_threshold_mult
  1.1,   # efficiency_threshold_mult
  1.02,  # miles_threshold_mult
  0.0001, # interaction_coeff
  3.5,   # log_bonus_coeff
  20.0,  # bug_base_bonus
  0.015  # bug_multiplier
)

# Evaluate baseline
baseline_error <- objective_function(baseline_params)
cat("Baseline average error: $", round(baseline_error, 2), "\n")

# Define parameter bounds for optimization
lower_bounds <- c(
  40,    # base_per_diem
  0.2,   # mileage_rate_1
  0.15,  # mileage_rate_2
  0.1,   # mileage_rate_3
  0.1,   # receipt_rate_1
  0.2,   # receipt_rate_2
  0.1,   # receipt_rate_3
  0.05,  # receipt_rate_4
  0.8,   # cluster_mult_0
  0.8,   # cluster_mult_1
  0.8,   # cluster_mult_2
  0.8,   # cluster_mult_3
  0.8,   # cluster_mult_4
  0.8,   # cluster_mult_5
  1.0,   # vacation_bonus_mult
  1.0,   # vacation_base_mult
  1.0,   # five_day_mult
  1.0,   # receipt_threshold_mult
  1.0,   # efficiency_threshold_mult
  1.0,   # miles_threshold_mult
  0.00001, # interaction_coeff
  1.0,   # log_bonus_coeff
  10.0,  # bug_base_bonus
  0.005  # bug_multiplier
)

upper_bounds <- c(
  80,    # base_per_diem
  0.8,   # mileage_rate_1
  0.6,   # mileage_rate_2
  0.8,   # mileage_rate_3
  0.6,   # receipt_rate_1
  0.8,   # receipt_rate_2
  0.6,   # receipt_rate_3
  0.3,   # receipt_rate_4
  1.3,   # cluster_mult_0
  1.3,   # cluster_mult_1
  1.3,   # cluster_mult_2
  1.5,   # cluster_mult_3
  1.3,   # cluster_mult_4
  1.3,   # cluster_mult_5
  1.3,   # vacation_bonus_mult
  1.2,   # vacation_base_mult
  1.3,   # five_day_mult
  1.2,   # receipt_threshold_mult
  1.3,   # efficiency_threshold_mult
  1.1,   # miles_threshold_mult
  0.001, # interaction_coeff
  6.0,   # log_bonus_coeff
  40.0,  # bug_base_bonus
  0.03   # bug_multiplier
)

# Run Differential Evolution optimization
cat("\nStarting Differential Evolution optimization...\n")
cat("This may take several minutes...\n")

set.seed(42)  # For reproducibility
de_result <- DEoptim(
  fn = objective_function,
  lower = lower_bounds,
  upper = upper_bounds,
  control = DEoptim.control(
    itermax = 100,  # Reduced for faster execution
    NP = 240,  # 10x parameter count (24 parameters)
    trace = 10,  # Print progress every 10 iterations
    parallelType = 0  # Disable parallel processing to avoid dependency issues
  )
)

# Extract best parameters
best_params <- de_result$optim$bestmem
best_error <- de_result$optim$bestval

cat("\n=== OPTIMIZATION RESULTS ===\n")
cat("Best average error: $", round(best_error, 2), "\n")
cat("Improvement: $", round(baseline_error - best_error, 2), "\n")
cat("Improvement percentage:", round((baseline_error - best_error) / baseline_error * 100, 1), "%\n")

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
  cat(sprintf("%-25s: %8.5f (was %8.5f)\n", param_names[i], best_params[i], baseline_params[i]))
}

# Analyze high-error cases with optimized parameters
cat("\n=== HIGH-ERROR CASE ANALYSIS ===\n")
errors_optimized <- numeric(nrow(inputs))
for (i in 1:nrow(inputs)) {
  prediction <- calculate_reimbursement(
    inputs$trip_duration_days[i],
    inputs$miles_traveled[i],
    inputs$total_receipts_amount[i],
    best_params
  )
  errors_optimized[i] <- abs(prediction - targets[i])
}

# Find top 10 highest error cases
error_indices <- order(errors_optimized, decreasing = TRUE)[1:10]

cat("Top 10 highest error cases with optimized parameters:\n")
for (i in 1:10) {
  idx <- error_indices[i]
  cat(sprintf("Case %d: %d days, %.0f miles, $%.2f receipts\n", 
              idx, inputs$trip_duration_days[idx], inputs$miles_traveled[idx], inputs$total_receipts_amount[idx]))
  cat(sprintf("  Expected: $%.2f, Got: $%.2f, Error: $%.2f\n", 
              targets[idx], targets[idx] - errors_optimized[idx], errors_optimized[idx]))
}

# Save optimized parameters to file for Python porting
optimized_config <- data.frame(
  parameter = param_names,
  value = best_params,
  baseline = baseline_params,
  improvement = baseline_params - best_params
)

write.csv(optimized_config, "optimized_parameters.csv", row.names = FALSE)

# Generate Python code snippet
cat("\n=== PYTHON CODE FOR OPTIMIZED PARAMETERS ===\n")
cat("# Optimized parameters from R DEoptim\n")
cat("class ModelConfig:\n")
cat("    def __init__(self):\n")
cat("        # Base calculation parameters (R-OPTIMIZED)\n")
cat(sprintf("        self.base_per_diem = %.5f\n", best_params[1]))
cat(sprintf("        self.mileage_rates = [%.5f, %.5f, %.5f]\n", best_params[2], best_params[3], best_params[4]))
cat(sprintf("        self.receipt_rates = [%.5f, %.5f, %.5f, %.5f]\n", best_params[5], best_params[6], best_params[7], best_params[8]))
cat("        \n")
cat("        # Cluster adjustment multipliers (R-OPTIMIZED)\n")
cat(sprintf("        self.cluster_multipliers = [%.5f, %.5f, %.5f, %.5f, %.5f, %.5f]\n", 
            best_params[9], best_params[10], best_params[11], best_params[12], best_params[13], best_params[14]))
cat(sprintf("        self.vacation_bonus_multiplier = %.5f\n", best_params[15]))
cat(sprintf("        self.vacation_base_multiplier = %.5f\n", best_params[16]))
cat("        \n")
cat("        # Threshold effect multipliers (R-OPTIMIZED)\n")
cat(sprintf("        self.five_day_multiplier = %.5f\n", best_params[17]))
cat(sprintf("        self.receipt_threshold_multiplier = %.5f\n", best_params[18]))
cat(sprintf("        self.efficiency_threshold_multiplier = %.5f\n", best_params[19]))
cat(sprintf("        self.miles_threshold_multiplier = %.5f\n", best_params[20]))
cat("        \n")
cat("        # Interaction coefficients (R-OPTIMIZED)\n")
cat(sprintf("        self.interaction_coeff = %.8f\n", best_params[21]))
cat(sprintf("        self.log_bonus_coeff = %.5f\n", best_params[22]))
cat("        \n")
cat("        # Rounding bug parameters (R-OPTIMIZED)\n")
cat(sprintf("        self.bug_base_bonus = %.5f\n", best_params[23]))
cat(sprintf("        self.bug_multiplier = %.5f\n", best_params[24]))

cat("\n=== OPTIMIZATION COMPLETE ===\n")
cat("Results saved to 'optimized_parameters.csv'\n")
cat("Ready for Phase 2: Python porting\n")
