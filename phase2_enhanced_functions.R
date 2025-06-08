# Phase 2: Enhanced Reimbursement Functions
# Based on pattern analysis findings

# Load base functions and optimized configuration
source("reimbursement_functions.R")

# Enhanced configuration with Phase 2 improvements
get_phase2_config <- function() {
  base_config <- get_optimized_config()
  
  # Add Phase 2 enhancements based on pattern analysis
  enhanced_config <- base_config
  
  # Enhanced pattern-specific adjustments
  enhanced_config$pattern_adjustments <- list(
    # Receipt ending patterns (major finding: 49/99 endings have huge errors)
    receipt_49_99_penalty = 0.75,  # Strong penalty for 49/99 endings
    receipt_0x_bonus = 1.05,       # Small bonus for 0X endings
    
    # Efficiency-based adjustments
    extreme_efficiency_penalty = 0.80,  # 300+ miles/day penalty
    very_low_efficiency_penalty = 0.85, # <50 miles/day penalty
    
    # Spending pattern adjustments
    extreme_spending_penalty = 0.78,    # $300+/day penalty
    very_high_spending_penalty = 0.88,  # $200-300/day penalty
    
    # Duration-specific adjustments
    one_day_trip_penalty = 0.70,        # 1-day trips are over-predicted
    short_trip_penalty = 0.85,          # 2-4 day trips
    
    # Combination pattern adjustments
    vacation_penalty_enhanced = 0.82,    # Enhanced vacation penalty
    one_day_long_distance_penalty = 0.75, # 1-day + high miles
    short_high_expense_bonus = 1.08      # Short trips with high expenses
  )
  
  # Enhanced thresholds based on analysis
  enhanced_config$enhanced_thresholds <- list(
    extreme_efficiency_threshold = 300,
    very_low_efficiency_threshold = 50,
    extreme_spending_threshold = 300,
    very_high_spending_threshold = 200,
    short_trip_threshold = 4,
    one_day_long_distance_miles = 800,
    short_high_expense_amount = 2000
  )
  
  return(enhanced_config)
}

# Enhanced feature engineering with Phase 2 insights
engineer_phase2_features <- function(trip_duration_days, miles_traveled, total_receipts_amount) {
  # Get base features
  base_features <- engineer_features(trip_duration_days, miles_traveled, total_receipts_amount)
  
  # Add Phase 2 enhanced features
  miles_per_day <- miles_traveled / trip_duration_days
  receipts_per_day <- total_receipts_amount / trip_duration_days
  
  # Receipt ending patterns (critical finding)
  receipt_cents <- (total_receipts_amount * 100) %% 100
  receipt_49_99_flag <- ifelse(receipt_cents %in% c(49, 99), 1, 0)
  receipt_0x_flag <- ifelse(receipt_cents < 10 & receipt_cents > 0, 1, 0)
  
  # Efficiency patterns
  extreme_efficiency_flag <- ifelse(miles_per_day >= 300, 1, 0)
  very_low_efficiency_flag <- ifelse(miles_per_day < 50, 1, 0)
  
  # Spending patterns
  extreme_spending_flag <- ifelse(receipts_per_day >= 300, 1, 0)
  very_high_spending_flag <- ifelse(receipts_per_day >= 200 & receipts_per_day < 300, 1, 0)
  
  # Duration patterns
  one_day_trip_flag <- ifelse(trip_duration_days == 1, 1, 0)
  short_trip_flag <- ifelse(trip_duration_days <= 4, 1, 0)
  
  # Combination patterns (from analysis)
  vacation_penalty_enhanced_flag <- ifelse(trip_duration_days >= 8 & receipts_per_day > 120, 1, 0)
  one_day_long_distance_flag <- ifelse(trip_duration_days == 1 & miles_traveled > 800, 1, 0)
  short_high_expense_flag <- ifelse(trip_duration_days <= 4 & total_receipts_amount > 2000, 1, 0)
  
  # Over-prediction risk factors
  over_prediction_risk <- ifelse(
    trip_duration_days <= 4 & miles_traveled < 600 & total_receipts_amount < 1200, 1, 0
  )
  
  # Add to base features
  enhanced_features <- c(base_features, list(
    receipt_49_99_flag = receipt_49_99_flag,
    receipt_0x_flag = receipt_0x_flag,
    extreme_efficiency_flag = extreme_efficiency_flag,
    very_low_efficiency_flag = very_low_efficiency_flag,
    extreme_spending_flag = extreme_spending_flag,
    very_high_spending_flag = very_high_spending_flag,
    one_day_trip_flag = one_day_trip_flag,
    short_trip_flag = short_trip_flag,
    vacation_penalty_enhanced_flag = vacation_penalty_enhanced_flag,
    one_day_long_distance_flag = one_day_long_distance_flag,
    short_high_expense_flag = short_high_expense_flag,
    over_prediction_risk = over_prediction_risk
  ))
  
  return(enhanced_features)
}

# Enhanced cluster determination with Phase 2 insights
determine_phase2_cluster <- function(features) {
  trip_duration <- features$trip_duration_days
  miles_traveled <- features$miles_traveled
  receipts <- features$total_receipts_amount
  miles_per_day <- features$miles_per_day
  receipts_per_day <- features$receipts_per_day
  
  # Enhanced cluster logic based on pattern analysis
  if (features$one_day_long_distance_flag == 1) {
    return(6)  # New cluster for 1-day long distance
  } else if (features$short_high_expense_flag == 1) {
    return(7)  # New cluster for short high expense
  } else if (features$extreme_efficiency_flag == 1) {
    return(8)  # New cluster for extreme efficiency
  } else if (features$very_low_efficiency_flag == 1 && trip_duration >= 8) {
    return(9)  # New cluster for long low efficiency
  } else if (features$vacation_penalty_enhanced_flag == 1) {
    return(10) # Enhanced vacation penalty cluster
  } else if (trip_duration <= 1.5 && miles_per_day > 500) {
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

# Enhanced reimbursement calculation with Phase 2 improvements
calculate_phase2_reimbursement <- function(trip_duration_days, miles_traveled, total_receipts_amount, config = NULL) {
  if (is.null(config)) {
    config <- get_phase2_config()
  }
  
  # Engineer enhanced features
  features <- engineer_phase2_features(trip_duration_days, miles_traveled, total_receipts_amount)
  
  # Determine enhanced cluster
  cluster <- determine_phase2_cluster(features)
  
  # Calculate base reimbursement (same as before)
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
  
  # Apply enhanced cluster adjustments
  if (cluster <= 5) {
    # Use original cluster multipliers
    adjusted_amount <- base_amount * config$cluster_multipliers[cluster + 1]
  } else {
    # New clusters with specific adjustments
    cluster_adjustments <- c(0.75, 1.08, 0.80, 0.85, 0.82)  # clusters 6-10
    adjusted_amount <- base_amount * cluster_adjustments[cluster - 5]
  }
  
  # Apply threshold effects
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
  
  # Apply Phase 2 pattern-specific adjustments
  pattern_adjusted <- threshold_adjusted
  
  # Critical: Receipt ending adjustments (major finding)
  if (features$receipt_49_99_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$receipt_49_99_penalty
  }
  
  if (features$receipt_0x_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$receipt_0x_bonus
  }
  
  # Efficiency pattern adjustments
  if (features$extreme_efficiency_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$extreme_efficiency_penalty
  }
  
  if (features$very_low_efficiency_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$very_low_efficiency_penalty
  }
  
  # Spending pattern adjustments
  if (features$extreme_spending_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$extreme_spending_penalty
  }
  
  if (features$very_high_spending_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$very_high_spending_penalty
  }
  
  # Duration pattern adjustments
  if (features$one_day_trip_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$one_day_trip_penalty
  } else if (features$short_trip_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$short_trip_penalty
  }
  
  # Combination pattern adjustments
  if (features$one_day_long_distance_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$one_day_long_distance_penalty
  }
  
  if (features$short_high_expense_flag == 1) {
    pattern_adjusted <- pattern_adjusted * config$pattern_adjustments$short_high_expense_bonus
  }
  
  # Apply rounding bug (enhanced)
  if (features$receipt_ends_49_99 == 1) {
    bug_bonus <- config$bug_base_bonus + (pattern_adjusted * config$bug_multiplier)
    final_amount <- pattern_adjusted + bug_bonus
  } else if (features$receipt_ends_special == 1) {
    bug_bonus <- config$bug_base_bonus * 0.5 + (pattern_adjusted * config$bug_multiplier * 0.5)
    final_amount <- pattern_adjusted + bug_bonus
  } else {
    final_amount <- pattern_adjusted
  }
  
  # Apply interaction effects
  interaction_bonus <- features$duration_spending_interaction * config$interaction_coeff
  log_bonus <- features$log_receipts * config$log_bonus_coeff
  
  final_amount <- final_amount + interaction_bonus + log_bonus
  
  # Ensure reasonable bounds
  final_amount <- max(50.0, min(final_amount, 5000.0))
  
  return(round(final_amount, 2))
}
