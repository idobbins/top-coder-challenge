#!/usr/bin/env Rscript

# Phase 2: Enhanced Pattern Recognition and Analysis
# Analyzes worst-performing cases to identify missing patterns

suppressMessages({
  library(dplyr)
  library(jsonlite)
  library(ggplot2)
})

# Source the functions
source("reimbursement_functions.R")

cat("=== PHASE 2: ENHANCED PATTERN RECOGNITION ===\n\n")

# Load test data
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  case_id = 1:length(test_data$input$trip_duration_days),
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)

cat("Loaded", nrow(test_data), "test cases for pattern analysis.\n\n")

# Get current predictions
config <- get_config()
predictions <- numeric(nrow(test_data))

for (i in 1:nrow(test_data)) {
  predictions[i] <- calculate_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i],
    config
  )
}

# Calculate errors and identify worst cases
test_data$predicted <- predictions
test_data$error <- abs(predictions - test_data$expected_output)
test_data$error_pct <- test_data$error / test_data$expected_output * 100

# Sort by error
test_data <- test_data[order(test_data$error, decreasing = TRUE), ]

cat("=== WORST CASE ANALYSIS ===\n")

# Analyze top 50 worst cases
worst_cases <- test_data[1:50, ]

cat("Top 50 worst cases analysis:\n")
cat("Average trip duration:", round(mean(worst_cases$trip_duration_days), 1), "days\n")
cat("Average miles:", round(mean(worst_cases$miles_traveled), 0), "miles\n")
cat("Average receipts: $", round(mean(worst_cases$total_receipts_amount), 2), "\n")
cat("Average miles per day:", round(mean(worst_cases$miles_traveled / worst_cases$trip_duration_days), 1), "\n")
cat("Average receipts per day: $", round(mean(worst_cases$total_receipts_amount / worst_cases$trip_duration_days), 2), "\n")
cat("Average error: $", round(mean(worst_cases$error), 2), "\n")
cat("Average error %:", round(mean(worst_cases$error_pct), 1), "%\n\n")

# Pattern analysis functions
analyze_receipt_patterns <- function(data) {
  cat("=== RECEIPT PATTERN ANALYSIS ===\n")
  
  # Analyze receipt endings
  data$receipt_cents <- (data$total_receipts_amount * 100) %% 100
  
  # Group by receipt endings
  ending_analysis <- data %>%
    mutate(
      ending_group = case_when(
        receipt_cents %in% c(49, 99) ~ "49/99 endings",
        receipt_cents %in% c(1, 51) ~ "01/51 endings", 
        receipt_cents == 0 ~ "Round dollars",
        receipt_cents < 10 ~ "0X endings",
        receipt_cents >= 90 ~ "9X endings",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(ending_group) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      avg_expected = mean(expected_output),
      avg_predicted = mean(predicted),
      .groups = 'drop'
    )
  
  print(ending_analysis)
  cat("\n")
  
  return(ending_analysis)
}

analyze_efficiency_patterns <- function(data) {
  cat("=== EFFICIENCY PATTERN ANALYSIS ===\n")
  
  data$miles_per_day <- data$miles_traveled / data$trip_duration_days
  data$receipts_per_day <- data$total_receipts_amount / data$trip_duration_days
  
  # Efficiency zones
  efficiency_analysis <- data %>%
    mutate(
      efficiency_zone = case_when(
        miles_per_day < 50 ~ "Very Low (<50)",
        miles_per_day < 100 ~ "Low (50-100)",
        miles_per_day < 150 ~ "Medium (100-150)",
        miles_per_day < 200 ~ "High (150-200)",
        miles_per_day < 300 ~ "Very High (200-300)",
        TRUE ~ "Extreme (300+)"
      )
    ) %>%
    group_by(efficiency_zone) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      avg_expected = mean(expected_output),
      avg_predicted = mean(predicted),
      .groups = 'drop'
    )
  
  print(efficiency_analysis)
  cat("\n")
  
  return(efficiency_analysis)
}

analyze_spending_patterns <- function(data) {
  cat("=== SPENDING PATTERN ANALYSIS ===\n")
  
  data$receipts_per_day <- data$total_receipts_amount / data$trip_duration_days
  
  spending_analysis <- data %>%
    mutate(
      spending_zone = case_when(
        receipts_per_day < 50 ~ "Very Low (<$50)",
        receipts_per_day < 100 ~ "Low ($50-100)",
        receipts_per_day < 150 ~ "Medium ($100-150)",
        receipts_per_day < 200 ~ "High ($150-200)",
        receipts_per_day < 300 ~ "Very High ($200-300)",
        TRUE ~ "Extreme ($300+)"
      )
    ) %>%
    group_by(spending_zone) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      avg_expected = mean(expected_output),
      avg_predicted = mean(predicted),
      .groups = 'drop'
    )
  
  print(spending_analysis)
  cat("\n")
  
  return(spending_analysis)
}

analyze_duration_patterns <- function(data) {
  cat("=== DURATION PATTERN ANALYSIS ===\n")
  
  duration_analysis <- data %>%
    mutate(
      duration_zone = case_when(
        trip_duration_days == 1 ~ "1 day",
        trip_duration_days == 2 ~ "2 days",
        trip_duration_days == 3 ~ "3 days",
        trip_duration_days == 4 ~ "4 days",
        trip_duration_days == 5 ~ "5 days",
        trip_duration_days == 6 ~ "6 days",
        trip_duration_days == 7 ~ "7 days",
        trip_duration_days %in% 8:10 ~ "8-10 days",
        trip_duration_days %in% 11:14 ~ "11-14 days",
        TRUE ~ "15+ days"
      )
    ) %>%
    group_by(duration_zone) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      avg_expected = mean(expected_output),
      avg_predicted = mean(predicted),
      .groups = 'drop'
    )
  
  print(duration_analysis)
  cat("\n")
  
  return(duration_analysis)
}

analyze_combination_patterns <- function(data) {
  cat("=== COMBINATION PATTERN ANALYSIS ===\n")
  
  data$miles_per_day <- data$miles_traveled / data$trip_duration_days
  data$receipts_per_day <- data$total_receipts_amount / data$trip_duration_days
  
  # High-error combination patterns
  combination_analysis <- data %>%
    mutate(
      pattern = case_when(
        # Kevin's patterns from interviews
        trip_duration_days == 5 & miles_per_day >= 180 & receipts_per_day < 100 ~ "Kevin's Sweet Spot",
        trip_duration_days >= 8 & receipts_per_day > 120 ~ "Vacation Penalty",
        miles_per_day > 200 & receipts_per_day < 80 ~ "High Mile Low Spend",
        miles_per_day < 100 & receipts_per_day > 100 ~ "Low Mile High Spend",
        
        # New patterns from worst cases
        trip_duration_days == 1 & miles_traveled > 800 ~ "1-Day Long Distance",
        trip_duration_days <= 4 & total_receipts_amount > 2000 ~ "Short High Expense",
        trip_duration_days >= 8 & miles_traveled < 200 ~ "Long Low Miles",
        trip_duration_days >= 10 & total_receipts_amount > 1500 ~ "Extended High Expense",
        
        TRUE ~ "Standard"
      )
    ) %>%
    group_by(pattern) %>%
    summarise(
      count = n(),
      avg_error = mean(error),
      avg_expected = mean(expected_output),
      avg_predicted = mean(predicted),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_error))
  
  print(combination_analysis)
  cat("\n")
  
  return(combination_analysis)
}

# Run all analyses
cat("Analyzing patterns in worst-performing cases...\n\n")

receipt_patterns <- analyze_receipt_patterns(worst_cases)
efficiency_patterns <- analyze_efficiency_patterns(worst_cases)
spending_patterns <- analyze_spending_patterns(worst_cases)
duration_patterns <- analyze_duration_patterns(worst_cases)
combination_patterns <- analyze_combination_patterns(test_data)

# Identify specific problematic cases
cat("=== SPECIFIC PROBLEMATIC PATTERNS ===\n")

# Cases where we significantly over-predict
over_predictions <- test_data[test_data$predicted > test_data$expected_output * 1.5, ]
cat("Cases where we over-predict by 50%+:", nrow(over_predictions), "\n")
if (nrow(over_predictions) > 0) {
  cat("Common characteristics:\n")
  cat("  Avg duration:", round(mean(over_predictions$trip_duration_days), 1), "days\n")
  cat("  Avg miles:", round(mean(over_predictions$miles_traveled), 0), "\n")
  cat("  Avg receipts: $", round(mean(over_predictions$total_receipts_amount), 2), "\n")
}

# Cases where we significantly under-predict
under_predictions <- test_data[test_data$predicted < test_data$expected_output * 0.5, ]
cat("\nCases where we under-predict by 50%+:", nrow(under_predictions), "\n")
if (nrow(under_predictions) > 0) {
  cat("Common characteristics:\n")
  cat("  Avg duration:", round(mean(under_predictions$trip_duration_days), 1), "days\n")
  cat("  Avg miles:", round(mean(under_predictions$miles_traveled), 0), "\n")
  cat("  Avg receipts: $", round(mean(under_predictions$total_receipts_amount), 2), "\n")
}

cat("\n=== PATTERN ANALYSIS COMPLETE ===\n")
cat("Use these insights to enhance feature engineering in Phase 2.\n")

# Save analysis results
save(
  test_data, worst_cases, receipt_patterns, efficiency_patterns, 
  spending_patterns, duration_patterns, combination_patterns,
  over_predictions, under_predictions,
  file = "phase2_analysis_results.RData"
)

cat("Analysis results saved to 'phase2_analysis_results.RData'\n")
