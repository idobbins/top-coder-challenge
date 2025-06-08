#!/usr/bin/env Rscript

# Conservative Evaluation Script
suppressMessages(library(dplyr))
source("conservative_enhanced.R")

# Load test data
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

config <- get_config()
cat("Running conservative evaluation on", nrow(test_data), "cases...\n")

predictions <- numeric(nrow(test_data))
errors <- numeric(nrow(test_data))

for (i in 1:nrow(test_data)) {
  prediction <- calculate_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i],
    config
  )
  
  predictions[i] <- prediction
  errors[i] <- abs(prediction - test_data$expected_output[i])
}

# Calculate metrics
exact_matches <- sum(errors <= 0.01)
close_matches <- sum(errors <= 1.00)
avg_error <- mean(errors)
score <- avg_error * 100 + (1000 - exact_matches) * 0.1

cat("\n=== CONSERVATIVE EVALUATION RESULTS ===\n")
cat("Exact matches (±$0.01):", exact_matches, "/", nrow(test_data), "(", round(exact_matches/nrow(test_data)*100, 1), "%)\n")
cat("Close matches (±$1.00):", close_matches, "/", nrow(test_data), "(", round(close_matches/nrow(test_data)*100, 1), "%)\n")
cat("Average error: $", round(avg_error, 2), "\n")
cat("Score:", round(score, 2), "(lower is better)\n")

# Compare with baseline
original_score <- 12000
improvement <- original_score - score
improvement_pct <- (improvement / original_score) * 100

cat("\n=== IMPROVEMENT ANALYSIS ===\n")
cat("Original score (estimated): ", original_score, "\n")
cat("Conservative score: ", round(score, 2), "\n")
cat("Improvement: ", round(improvement, 2), " points\n")
cat("Improvement percentage: ", round(improvement_pct, 1), "%\n")

cat("\nConservative evaluation complete!\n")
