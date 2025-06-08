# Phase 2 Optimized Configuration
# Generated on: 2025-06-07 18:46:45.388996

get_phase2_optimized_config <- function() {
  base_config <- get_optimized_config()
  enhanced_config <- base_config

  # Optimized Phase 2 pattern adjustments
  enhanced_config$pattern_adjustments <- list(
    receipt_49_99_penalty = 0.889465176157098,
    receipt_0x_bonus = 1.0465771414998,
    extreme_efficiency_penalty = 1.09422315616253,
    very_low_efficiency_penalty = 1.08203069021967,
    extreme_spending_penalty = 1.00155928656317,
    very_high_spending_penalty = 1.04171154033708,
    one_day_trip_penalty = 1.02545506700287,
    short_trip_penalty = 0.994276098146114,
    one_day_long_distance_penalty = 1.00445049779358,
    short_high_expense_bonus = 0.959087671358912
  )

  return(enhanced_config)
}

