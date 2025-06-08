#!/usr/bin/env Rscript
# Quick micro-tuning of key parameters
suppressMessages(library(dplyr))
source("reimbursement_functions.R")

cat("=== MICRO-TUNING TEST ===\n")

# Load test data
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

# Test function
test_config <- function(base_per_diem_mult, mileage_mult) {
  config <- get_config()
  config$base_per_diem <- config$base_per_diem * base_per_diem_mult
  config$mileage_rates[1] <- config$mileage_rates[1] * mileage_mult
  
  predictions <- numeric(nrow(test_data))
  for (i in 1:nrow(test_data)) {
    predictions[i] <- calculate_reimbursement(
      test_data$trip_duration_days[i],
      test_data$miles_traveled[i],
      test_data$total_receipts_amount[i],
      config
    )
  }
  
  errors <- abs(predictions - test_data$expected_output)
  avg_error <- mean(errors)
  score <- sum(errors^2)
  
  return(list(avg_error = avg_error, score = score))
}

# Quick tests
cat("Testing micro-adjustments...\n")
baseline <- test_config(1.0, 1.0)
cat("Baseline: Avg Error $", round(baseline$avg_error, 2), "\n")

# Test small adjustments
tests <- list(
  c(0.98, 1.0),   # -2% per diem
  c(1.02, 1.0),   # +2% per diem
  c(1.0, 0.98),   # -2% mileage
  c(1.0, 1.02),   # +2% mileage
  c(0.99, 1.01),  # -1% per diem, +1% mileage
  c(1.01, 0.99)   # +1% per diem, -1% mileage
)

best_error <- baseline$avg_error
best_params <- c(1.0, 1.0)

for (i in 1:length(tests)) {
  result <- test_config(tests[[i]][1], tests[[i]][2])
  cat("Test", i, ": per_diem", tests[[i]][1], "mileage", tests[[i]][2], 
      "-> Avg Error $", round(result$avg_error, 2), "\n")
  
  if (result$avg_error < best_error) {
    best_error <- result$avg_error
    best_params <- tests[[i]]
    cat("  ✅ NEW BEST!\n")
  }
}

cat("\n=== RESULTS ===\n")
cat("Best params: per_diem", best_params[1], "mileage", best_params[2], "\n")
cat("Best avg error: $", round(best_error, 2), "\n")
cat("Improvement: $", round(baseline$avg_error - best_error, 2), "\n")
