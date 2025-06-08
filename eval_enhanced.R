#!/usr/bin/env Rscript

# Enhanced Evaluation Script for R Implementation
# Tests the enhanced reimbursement functions

# Load required libraries
suppressMessages(library(dplyr))

# Source the enhanced calculation functions
source("enhanced_reimbursement_functions.R")

# Load test data
cat("Loading test data...\n")
test_data <- jsonlite::fromJSON("public_cases.json")
# Convert to data frame format
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

# Get the enhanced config
config <- get_config()

cat("Running enhanced evaluation on", nrow(test_data), "cases...\n")

# Calculate predictions
predictions <- numeric(nrow(test_data))
errors <- numeric(nrow(test_data))

start_time <- Sys.time()

for (i in 1:nrow(test_data)) {
  prediction <- calculate_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i],
    config
  )
  
  predictions[i] <- prediction
  errors[i] <- abs(prediction - test_data$expected_output[i])
  
  # Progress indicator
  if (i %% 100 == 0) {
    cat("Processed", i, "/", nrow(test_data), "cases\n")
  }
}

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Calculate metrics
exact_matches <- sum(errors <= 0.01)
close_matches <- sum(errors <= 1.00)
avg_error <- mean(errors)
median_error <- median(errors)
max_error <- max(errors)
rmse <- sqrt(mean(errors^2))

# Score calculation (lower is better) - matches eval.sh formula
score <- avg_error * 100 + (1000 - exact_matches) * 0.1

cat("\n=== ENHANCED EVALUATION RESULTS ===\n")
cat("Runtime:", round(runtime, 2), "seconds\n")
cat("Exact matches (±$0.01):", exact_matches, "/", nrow(test_data), "(", round(exact_matches/nrow(test_data)*100, 1), "%)\n")
cat("Close matches (±$1.00):", close_matches, "/", nrow(test_data), "(", round(close_matches/nrow(test_data)*100, 1), "%)\n")
cat("Average error: $", round(avg_error, 2), "\n")
cat("Median error: $", round(median_error, 2), "\n")
cat("Max error: $", round(max_error, 2), "\n")
cat("RMSE: $", round(rmse, 2), "\n")
cat("Score:", round(score, 2), "(lower is better)\n")

# Compare with baseline (assuming original score was around 12000)
original_score <- 12000
improvement <- original_score - score
improvement_pct <- (improvement / original_score) * 100

cat("\n=== IMPROVEMENT ANALYSIS ===\n")
cat("Original score (estimated): ", original_score, "\n")
cat("Enhanced score: ", round(score, 2), "\n")
cat("Improvement: ", round(improvement, 2), " points\n")
cat("Improvement percentage: ", round(improvement_pct, 1), "%\n")

# Find worst cases for analysis
worst_cases_idx <- order(errors, decreasing = TRUE)[1:5]

cat("\n=== TOP 5 WORST CASES ===\n")
for (i in 1:5) {
  idx <- worst_cases_idx[i]
  cat(sprintf("Case %d: %.0f days, %.0f miles, $%.2f receipts\n", 
              idx, test_data$trip_duration_days[idx], 
              test_data$miles_traveled[idx], 
              test_data$total_receipts_amount[idx]))
  cat(sprintf("  Expected: $%.2f, Got: $%.2f, Error: $%.2f\n", 
              test_data$expected_output[idx], 
              predictions[idx], 
              errors[idx]))
}

cat("\nEnhanced evaluation complete!\n")
