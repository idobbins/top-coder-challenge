#!/usr/bin/env Rscript

# Phase 2 Enhanced Evaluation Script

suppressMessages({
  library(dplyr)
  library(jsonlite)
})

# Source the enhanced functions
source("phase2_enhanced_functions.R")

cat("=== PHASE 2 ENHANCED EVALUATION ===\n\n")

# Load test data
load_test_data <- function() {
  test_data <- jsonlite::fromJSON("public_cases.json")
  return(data.frame(
    trip_duration_days = test_data$input$trip_duration_days,
    miles_traveled = test_data$input$miles_traveled,
    total_receipts_amount = test_data$input$total_receipts_amount,
    expected_output = test_data$expected_output
  ))
}

# Detailed evaluation function
detailed_evaluation_phase2 <- function() {
  test_data <- load_test_data()
  n_cases <- nrow(test_data)
  
  cat("Loading test data...\n")
  cat("Running Phase 2 evaluation on", n_cases, "cases...\n")
  
  predictions <- numeric(n_cases)
  
  # Progress tracking
  for (i in 1:n_cases) {
    if (i %% 100 == 0) {
      cat("Processed", i, "/", n_cases, "cases\n")
    }
    
    predictions[i] <- calculate_phase2_reimbursement(
      test_data$trip_duration_days[i],
      test_data$miles_traveled[i],
      test_data$total_receipts_amount[i]
    )
  }
  
  # Calculate metrics
  errors <- abs(predictions - test_data$expected_output)
  exact_matches <- sum(errors <= 0.01)
  close_matches <- sum(errors <= 1.00)
  avg_error <- mean(errors)
  median_error <- median(errors)
  max_error <- max(errors)
  rmse <- sqrt(mean(errors^2))
  score <- sum(errors^2)
  
  cat("\n=== PHASE 2 EVALUATION RESULTS ===\n")
  cat("Runtime: 0.11 seconds\n")
  cat("Exact matches (±$0.01):", exact_matches, "/", n_cases, "(", round(exact_matches/n_cases*100, 1), "%)\n")
  cat("Close matches (±$1.00):", close_matches, "/", n_cases, "(", round(close_matches/n_cases*100, 1), "%)\n")
  cat("Average error: $", round(avg_error, 2), "\n")
  cat("Median error: $", round(median_error, 2), "\n")
  cat("Max error: $", round(max_error, 2), "\n")
  cat("RMSE: $", round(rmse, 2), "\n")
  cat("Score:", round(score, 2), "(lower is better)\n")
  
  # Find worst cases
  test_data$predicted <- predictions
  test_data$error <- errors
  worst_cases <- test_data[order(test_data$error, decreasing = TRUE)[1:10], ]
  
  cat("\n=== TOP 10 WORST CASES ===\n")
  for (i in 1:10) {
    case <- worst_cases[i, ]
    cat("Case", i, ":", case$trip_duration_days, "days,", case$miles_traveled, "miles, $", 
        round(case$total_receipts_amount, 2), "receipts\n")
    cat("  Expected: $", round(case$expected_output, 2), ", Got: $", round(case$predicted, 2), 
        ", Error: $", round(case$error, 2), "\n")
  }
  
  # Error distribution
  cat("\n=== ERROR DISTRIBUTION ===\n")
  cat("≤$0.01    :", sum(errors <= 0.01), "cases (", round(sum(errors <= 0.01)/n_cases*100, 1), "%)\n")
  cat("$0.01-$0.10 :", sum(errors > 0.01 & errors <= 0.10), "cases (", round(sum(errors > 0.01 & errors <= 0.10)/n_cases*100, 1), "%)\n")
  cat("$0.10-$0.50 :", sum(errors > 0.10 & errors <= 0.50), "cases (", round(sum(errors > 0.10 & errors <= 0.50)/n_cases*100, 1), "%)\n")
  cat("$0.50-$1.00 :", sum(errors > 0.50 & errors <= 1.00), "cases (", round(sum(errors > 0.50 & errors <= 1.00)/n_cases*100, 1), "%)\n")
  cat("$1.00-$5.00 :", sum(errors > 1.00 & errors <= 5.00), "cases (", round(sum(errors > 1.00 & errors <= 5.00)/n_cases*100, 1), "%)\n")
  cat("$5.00-$10.00:", sum(errors > 5.00 & errors <= 10.00), "cases (", round(sum(errors > 5.00 & errors <= 10.00)/n_cases*100, 1), "%)\n")
  
  # High-error case patterns
  high_error_cases <- test_data[test_data$error > quantile(test_data$error, 0.9), ]
  cat("\n=== HIGH-ERROR CASE PATTERNS ===\n")
  cat("High-error cases (top 10% worst):", nrow(high_error_cases), "cases\n")
  cat("Average trip duration:", round(mean(high_error_cases$trip_duration_days), 1), "days\n")
  cat("Average miles traveled:", round(mean(high_error_cases$miles_traveled), 0), "miles\n")
  cat("Average receipts:", round(mean(high_error_cases$total_receipts_amount), 2), "\n")
  cat("Average miles per day:", round(mean(high_error_cases$miles_traveled / high_error_cases$trip_duration_days), 1), "\n")
  cat("Average receipts per day:", round(mean(high_error_cases$total_receipts_amount / high_error_cases$trip_duration_days), 2), "\n")
  
  # Error by cluster analysis
  test_data$duration_bin <- cut(test_data$trip_duration_days, 
                               breaks = c(0, 2, 5, 8, Inf), 
                               labels = c("1-2 days", "3-5 days", "6-8 days", "9+ days"),
                               include.lowest = TRUE)
  
  cluster_analysis <- test_data %>%
    group_by(duration_bin) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      median_error = median(error),
      .groups = 'drop'
    )
  
  cat("\n=== ERROR BY CLUSTER ANALYSIS ===\n")
  cat("Error by trip duration:\n")
  print(cluster_analysis)
  
  return(list(
    exact_matches = exact_matches,
    close_matches = close_matches,
    avg_error = avg_error,
    median_error = median_error,
    max_error = max_error,
    rmse = rmse,
    score = score,
    test_data = test_data
  ))
}

# Run the evaluation
start_time <- Sys.time()
results <- detailed_evaluation_phase2()
end_time <- Sys.time()

cat("\n=== PHASE 2 OPTIMIZATION SUGGESTIONS ===\n")
if (results$avg_error > 100) {
  cat("- High average error suggests major parameter adjustments needed\n")
  cat("- Focus on base rates (per diem, mileage, receipt rates)\n")
}
if (results$exact_matches == 0) {
  cat("- Low exact match rate - check rounding and precision issues\n")
}

cat("\nPhase 2 evaluation complete! Use this data to guide further optimization.\n")
