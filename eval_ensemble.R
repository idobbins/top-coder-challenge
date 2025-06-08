#!/usr/bin/env Rscript
# Quick Ensemble Evaluation
suppressMessages(library(dplyr))
source("ensemble_reimbursement.R")

cat("=== ENSEMBLE EVALUATION ===\n")

# Load test data
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

cat("Running ensemble on", nrow(test_data), "cases...\n")

# Calculate predictions
predictions <- numeric(nrow(test_data))
start_time <- Sys.time()

for (i in 1:nrow(test_data)) {
  predictions[i] <- calculate_ensemble_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i]
  )
  
  if (i %% 200 == 0) cat("Processed", i, "/", nrow(test_data), "\n")
}

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Calculate metrics
errors <- abs(predictions - test_data$expected_output)
exact_matches <- sum(errors <= 0.01)
close_matches <- sum(errors <= 1.00)
avg_error <- mean(errors)
median_error <- median(errors)
max_error <- max(errors)
rmse <- sqrt(mean(errors^2))
score <- sum(errors^2)

cat("\n=== ENSEMBLE RESULTS ===\n")
cat("Runtime:", round(runtime, 2), "seconds\n")
cat("Exact matches (±$0.01):", exact_matches, "/", nrow(test_data), "(", round(exact_matches/nrow(test_data)*100, 1), "%)\n")
cat("Close matches (±$1.00):", close_matches, "/", nrow(test_data), "(", round(close_matches/nrow(test_data)*100, 1), "%)\n")
cat("Average error: $", round(avg_error, 2), "\n")
cat("Median error: $", round(median_error, 2), "\n")
cat("Max error: $", round(max_error, 2), "\n")
cat("RMSE: $", round(rmse, 2), "\n")
cat("Score:", round(score, 2), "(lower is better)\n")

cat("\n=== COMPARISON WITH BASELINE ===\n")
cat("Original Phase 1: Avg Error $109.80, Score 11080.24\n")
cat("Ensemble:        Avg Error $", round(avg_error, 2), ", Score", round(score, 2), "\n")

improvement_avg <- 109.80 - avg_error
improvement_score <- 11080.24 - score

if (improvement_avg > 0) {
  cat("✅ IMPROVEMENT: -$", round(improvement_avg, 2), "avg error, -", round(improvement_score, 2), "score\n")
} else {
  cat("❌ No improvement: +$", round(abs(improvement_avg), 2), "avg error\n")
}
