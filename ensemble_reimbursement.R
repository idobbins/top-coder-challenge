# Quick Ensemble Approach - Combining Multiple Configurations
# Load base functions
source("reimbursement_functions.R")
source("optimized_config.R")

# Create ensemble configuration
get_ensemble_config <- function() {
  base_config <- get_optimized_config()
  
  # Create 3 slight variations of the optimized config
  config1 <- base_config
  config1$base_per_diem <- base_config$base_per_diem * 1.02  # +2%
  config1$mileage_rates[1] <- base_config$mileage_rates[1] * 0.98  # -2%
  
  config2 <- base_config
  config2$base_per_diem <- base_config$base_per_diem * 0.98  # -2%
  config2$receipt_rates[1] <- base_config$receipt_rates[1] * 1.03  # +3%
  
  config3 <- base_config
  config3$mileage_rates[2] <- base_config$mileage_rates[2] * 1.05  # +5%
  config3$receipt_threshold <- base_config$receipt_threshold * 0.95  # -5%
  
  return(list(
    base = base_config,
    variant1 = config1,
    variant2 = config2,
    variant3 = config3
  ))
}

# Ensemble calculation function
calculate_ensemble_reimbursement <- function(trip_duration_days, miles_traveled, total_receipts_amount) {
  configs <- get_ensemble_config()
  
  # Calculate with each configuration
  result1 <- calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount, configs$base)
  result2 <- calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount, configs$variant1)
  result3 <- calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount, configs$variant2)
  result4 <- calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount, configs$variant3)
  
  # Weighted average (base config gets more weight)
  ensemble_result <- (result1 * 0.5 + result2 * 0.2 + result3 * 0.2 + result4 * 0.1)
  
  return(round(ensemble_result, 2))
}
