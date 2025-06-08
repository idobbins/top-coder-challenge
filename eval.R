#!/usr/bin/env Rscript

# Fast Evaluation Script for R Implementation
# Provides quick feedback for parameter optimization

# Load required libraries
suppressMessages(library(dplyr))

# Source the calculation functions
source("reimbursement_functions.R")

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

# Get the config
config <- get_config()

cat("Running evaluation on", nrow(test_data), "cases...\n")

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

cat("\n=== EVALUATION RESULTS ===\n")
cat("Runtime:", round(runtime, 2), "seconds\n")
cat("Exact matches (±$0.01):", exact_matches, "/", nrow(test_data), "(", round(exact_matches/nrow(test_data)*100, 1), "%)\n")
cat("Close matches (±$1.00):", close_matches, "/", nrow(test_data), "(", round(close_matches/nrow(test_data)*100, 1), "%)\n")
cat("Average error: $", round(avg_error, 2), "\n")
cat("Median error: $", round(median_error, 2), "\n")
cat("Max error: $", round(max_error, 2), "\n")
cat("RMSE: $", round(rmse, 2), "\n")
cat("Score:", round(score, 2), "(lower is better)\n")

# Find worst cases for analysis
worst_cases_idx <- order(errors, decreasing = TRUE)[1:10]

cat("\n=== TOP 10 WORST CASES ===\n")
for (i in 1:10) {
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

# Error distribution analysis
cat("\n=== ERROR DISTRIBUTION ===\n")
error_ranges <- c(0.01, 0.1, 0.5, 1.0, 5.0, 10.0, Inf)
error_labels <- c("≤$0.01", "$0.01-$0.10", "$0.10-$0.50", "$0.50-$1.00", "$1.00-$5.00", "$5.00-$10.00", ">$10.00")

for (i in 1:(length(error_ranges)-1)) {
  if (i == 1) {
    count <- sum(errors <= error_ranges[i])
  } else {
    count <- sum(errors > error_ranges[i-1] & errors <= error_ranges[i])
  }
  cat(sprintf("%-12s: %4d cases (%5.1f%%)\n", error_labels[i], count, count/nrow(test_data)*100))
}

# Feature analysis for high-error cases
cat("\n=== HIGH-ERROR CASE PATTERNS ===\n")
high_error_threshold <- quantile(errors, 0.9)
high_error_cases <- which(errors > high_error_threshold)

if (length(high_error_cases) > 0) {
  high_error_data <- test_data[high_error_cases, ]
  
  cat("High-error cases (top 10% worst):", length(high_error_cases), "cases\n")
  cat("Average trip duration:", round(mean(high_error_data$trip_duration_days), 1), "days\n")
  cat("Average miles traveled:", round(mean(high_error_data$miles_traveled), 0), "miles\n")
  cat("Average receipts:", round(mean(high_error_data$total_receipts_amount), 2), "\n")
  cat("Average miles per day:", round(mean(high_error_data$miles_traveled / high_error_data$trip_duration_days), 1), "\n")
  cat("Average receipts per day:", round(mean(high_error_data$total_receipts_amount / high_error_data$trip_duration_days), 2), "\n")
}

# Cluster analysis for errors
cat("\n=== ERROR BY CLUSTER ANALYSIS ===\n")
cluster_errors <- data.frame(
  trip_duration = test_data$trip_duration_days,
  miles_traveled = test_data$miles_traveled,
  receipts = test_data$total_receipts_amount,
  error = errors
)

# Analyze by trip duration
duration_bins <- cut(cluster_errors$trip_duration, breaks = c(0, 2, 5, 8, Inf), 
                    labels = c("1-2 days", "3-5 days", "6-8 days", "9+ days"))
duration_analysis <- cluster_errors %>%
  mutate(duration_bin = duration_bins) %>%
  group_by(duration_bin) %>%
  summarise(
    count = n(),
    avg_error = mean(error),
    median_error = median(error),
    .groups = 'drop'
  )

cat("Error by trip duration:\n")
print(duration_analysis)

# Quick optimization suggestions
cat("\n=== OPTIMIZATION SUGGESTIONS ===\n")
if (avg_error > 10) {
  cat("- High average error suggests major parameter adjustments needed\n")
  cat("- Focus on base rates (per diem, mileage, receipt rates)\n")
} else if (avg_error > 5) {
  cat("- Moderate error suggests fine-tuning cluster multipliers\n")
  cat("- Check threshold values and interaction coefficients\n")
} else {
  cat("- Low average error - focus on edge cases and specific patterns\n")
  cat("- Consider ensemble methods or advanced feature engineering\n")
}

if (exact_matches < 500) {
  cat("- Low exact match rate - check rounding and precision issues\n")
}

cat("\nEvaluation complete! Use this data to guide your next optimization iteration.\n")
