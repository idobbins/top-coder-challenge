# Reimbursement Calculation Functions
# Separated from command-line script for use in evaluation

# Smart optimized parameters with configurable thresholds
get_config <- function() {
  return(list(
    # Basic calculation parameters
    base_per_diem = 67,
    mileage_rates = c(0.6, 0.6, 0.4),
    receipt_rates = c(0.5, 0.55, 0.6, 0.1),
    
    # Tier boundaries (optimized)
    mileage_tiers = c(120, 600),  # Optimized: First 120 miles, next 480 miles, then remainder
    receipt_tiers = c(50, 500, 1500),  # First $50, next $450, next $1000, then remainder
    
    # Threshold values (optimized)
    receipt_threshold = 660.54,
    efficiency_threshold = 187.01,
    miles_threshold = 450,  # Optimized from 473.8
    
    # Feature engineering thresholds
    efficiency_zones = c(100, 180, 220, 300),  # miles per day boundaries
    spending_categories = c(75, 120),  # receipts per day boundaries
    sweet_spot_duration = c(4, 6),  # trip duration sweet spot range
    
    # Cluster definition parameters
    cluster_boundaries = list(
      very_short_high_eff = c(1.5, 500),  # trip_duration <= 1.5, miles_per_day > 500
      long_low_eff = c(8, 50),  # trip_duration > 8, miles_per_day < 50
      medium_high_spend = c(3, 5, 1500),  # trip_duration 3-5, receipts > 1500
      medium_high_eff = c(4, 7, 150, 800),  # trip_duration 4-7, miles_per_day > 150, receipts < 800
      long_high_spend = c(8, 1200)  # trip_duration > 8, receipts > 1200
    ),
    
    # Multipliers and adjustments
    cluster_multipliers = c(1.01277, 0.96982, 1.01280, 1.01430, 1.00000, 1.01208),
    vacation_bonus_multiplier = 1.00000,
    vacation_base_multiplier = 1.00000,
    five_day_multiplier = 1.10000,
    receipt_threshold_multiplier = 1.00000,
    efficiency_threshold_multiplier = 1.00000,
    miles_threshold_multiplier = 0.95000,
    
    # Advanced features (optimized)
    interaction_coeff = -0.001,
    log_bonus_coeff = 5,  # Optimized from 0
    bug_base_bonus = 0.00000,
    bug_multiplier = 0.00000,
    
    # Rounding bug parameters
    bug_cents_values = c(49, 99)
  ))
}

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
calculate_reimbursement <- function(trip_duration_days, miles_traveled, total_receipts_amount, config = NULL) {
  if (is.null(config)) {
    config <- get_config()
  }
  
  # Engineer features
  features <- engineer_features(trip_duration_days, miles_traveled, total_receipts_amount)
  
  # Determine cluster
  cluster <- determine_cluster(features)
  
  # Calculate base reimbursement
  base_per_diem_amount <- config$base_per_diem * trip_duration_days
  
  # Mileage calculation with configurable tiers
  tier1 <- config$mileage_tiers[1]
  tier2 <- config$mileage_tiers[2]
  
  if (miles_traveled <= tier1) {
    mileage_reimbursement <- miles_traveled * config$mileage_rates[1]
  } else if (miles_traveled <= tier2) {
    mileage_reimbursement <- tier1 * config$mileage_rates[1] + (miles_traveled - tier1) * config$mileage_rates[2]
  } else {
    mileage_reimbursement <- tier1 * config$mileage_rates[1] + (tier2 - tier1) * config$mileage_rates[2] + (miles_traveled - tier2) * config$mileage_rates[3]
  }
  
  # Receipt reimbursement with configurable tiers
  rtier1 <- config$receipt_tiers[1]
  rtier2 <- config$receipt_tiers[2]
  rtier3 <- config$receipt_tiers[3]
  
  if (total_receipts_amount <= rtier1) {
    receipt_reimbursement <- total_receipts_amount * config$receipt_rates[1]
  } else if (total_receipts_amount <= rtier2) {
    receipt_reimbursement <- rtier1 * config$receipt_rates[1] + (total_receipts_amount - rtier1) * config$receipt_rates[2]
  } else if (total_receipts_amount <= rtier3) {
    receipt_reimbursement <- rtier1 * config$receipt_rates[1] + (rtier2 - rtier1) * config$receipt_rates[2] + (total_receipts_amount - rtier2) * config$receipt_rates[3]
  } else {
    receipt_reimbursement <- rtier1 * config$receipt_rates[1] + (rtier2 - rtier1) * config$receipt_rates[2] + (rtier3 - rtier2) * config$receipt_rates[3] + (total_receipts_amount - rtier3) * config$receipt_rates[4]
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
  
  if (total_receipts_amount > config$receipt_threshold) {
    threshold_adjusted <- threshold_adjusted * config$receipt_threshold_multiplier
  }
  
  if (features$miles_per_day > config$efficiency_threshold) {
    threshold_adjusted <- threshold_adjusted * config$efficiency_threshold_multiplier
  }
  
  if (miles_traveled > config$miles_threshold) {
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
