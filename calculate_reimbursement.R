#!/usr/bin/env Rscript

# Black Box Challenge - Legacy Reimbursement System Replica (R Implementation)
# Optimized version using DEoptim results from optimize_parameters.R

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  cat("Usage: Rscript calculate_reimbursement.R <trip_duration_days> <miles_traveled> <total_receipts_amount>\n")
  quit(status = 1)
}

# Parse inputs
trip_duration_days <- as.numeric(args[1])
miles_traveled <- as.numeric(args[2])
total_receipts_amount <- as.numeric(args[3])

# Optimized parameters from DEoptim (from optimized_parameters.csv)
config <- list(
  base_per_diem = 45.5195880490612,
  mileage_rates = c(0.787071175593883, 0.45114227601327, 0.200831651920261),
  receipt_rates = c(0.399975746217208, 0.3045420665708, 0.436856841815577, 0.121882355942393),
  cluster_multipliers = c(1.14756663634639, 1.11176444876928, 1.20275399349877, 
                         1.19116480573546, 1.07688362098627, 1.2140054738624),
  vacation_bonus_multiplier = 1.04678047616035,
  vacation_base_multiplier = 1.07234985943884,
  five_day_multiplier = 1.08250167328268,
  receipt_threshold_multiplier = 1.19666525936238,
  efficiency_threshold_multiplier = 1.01898035206799,
  miles_threshold_multiplier = 1.02133465968143,
  interaction_coeff = 0.000555204802994334,
  log_bonus_coeff = 5.12776389976777,
  bug_base_bonus = 20.6993690179661,
  bug_multiplier = 0.00616844543255866
)

# Feature engineering function
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
calculate_reimbursement <- function(trip_duration_days, miles_traveled, total_receipts_amount, config) {
  # Engineer features
  features <- engineer_features(trip_duration_days, miles_traveled, total_receipts_amount)
  
  # Determine cluster
  cluster <- determine_cluster(features)
  
  # Calculate base reimbursement
  base_per_diem_amount <- config$base_per_diem * trip_duration_days
  
  # Mileage calculation with optimized rates
  if (miles_traveled <= 100) {
    mileage_reimbursement <- miles_traveled * config$mileage_rates[1]
  } else if (miles_traveled <= 500) {
    mileage_reimbursement <- 100 * config$mileage_rates[1] + (miles_traveled - 100) * config$mileage_rates[2]
  } else {
    mileage_reimbursement <- 100 * config$mileage_rates[1] + 400 * config$mileage_rates[2] + (miles_traveled - 500) * config$mileage_rates[3]
  }
  
  # Receipt reimbursement with optimized rates
  if (total_receipts_amount <= 50) {
    receipt_reimbursement <- total_receipts_amount * config$receipt_rates[1]
  } else if (total_receipts_amount <= 500) {
    receipt_reimbursement <- 50 * config$receipt_rates[1] + (total_receipts_amount - 50) * config$receipt_rates[2]
  } else if (total_receipts_amount <= 1500) {
    receipt_reimbursement <- 50 * config$receipt_rates[1] + 450 * config$receipt_rates[2] + (total_receipts_amount - 500) * config$receipt_rates[3]
  } else {
    receipt_reimbursement <- 50 * config$receipt_rates[1] + 450 * config$receipt_rates[2] + 1000 * config$receipt_rates[3] + (total_receipts_amount - 1500) * config$receipt_rates[4]
  }
  
  base_amount <- base_per_diem_amount + mileage_reimbursement + receipt_reimbursement
  
  # Apply cluster adjustments with optimized multipliers
  if (cluster == 4 && features$vacation_penalty == 1) {
    adjusted_amount <- base_amount * config$vacation_bonus_multiplier
  } else if (cluster == 4) {
    adjusted_amount <- base_amount * config$vacation_base_multiplier
  } else {
    adjusted_amount <- base_amount * config$cluster_multipliers[cluster + 1]
  }
  
  # Apply threshold effects with optimized multipliers
  threshold_adjusted <- adjusted_amount
  
  if (features$is_5_day_trip == 1) {
    threshold_adjusted <- threshold_adjusted * config$five_day_multiplier
  }
  
  if (total_receipts_amount > 660.54) {
    threshold_adjusted <- threshold_adjusted * config$receipt_threshold_multiplier
  }
  
  if (features$miles_per_day > 187.01) {
    threshold_adjusted <- threshold_adjusted * config$efficiency_threshold_multiplier
  }
  
  if (miles_traveled > 473.8) {
    threshold_adjusted <- threshold_adjusted * config$miles_threshold_multiplier
  }
  
  # Apply rounding bug with optimized parameters
  if (features$receipt_ends_49_99 == 1) {
    bug_bonus <- config$bug_base_bonus + (threshold_adjusted * config$bug_multiplier)
    final_amount <- threshold_adjusted + bug_bonus
  } else {
    final_amount <- threshold_adjusted
  }
  
  # Apply interaction effects with optimized coefficients
  interaction_bonus <- features$duration_spending_interaction * config$interaction_coeff
  log_bonus <- features$log_receipts * config$log_bonus_coeff
  
  final_amount <- final_amount + interaction_bonus + log_bonus
  
  # Ensure reasonable bounds
  final_amount <- max(50.0, min(final_amount, 5000.0))
  
  return(round(final_amount, 2))
}

# Calculate and output the result
result <- calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount, config)
cat(result, "\n")
