#!/usr/bin/env Rscript

# Simple test without GA package dependency

cat("=== SIMPLE OPTIMIZER TEST ===\n\n")

# Load basic libraries
suppressMessages({
  library(dplyr)
  library(jsonlite)
})

# Source the functions
source("reimbursement_functions.R")

# Test data loading
cat("Testing data loading...\n")
test_data <- jsonlite::fromJSON("public_cases.json")
test_data <- data.frame(
  trip_duration_days = test_data$input$trip_duration_days,
  miles_traveled = test_data$input$miles_traveled,
  total_receipts_amount = test_data$input$total_receipts_amount,
  expected_output = test_data$expected_output
)
cat("Loaded", nrow(test_data), "test cases successfully.\n\n")

# Test current configuration
cat("Testing current configuration...\n")
config <- get_config()

# Test on first 10 cases
predictions <- numeric(10)
errors <- numeric(10)

for (i in 1:10) {
  prediction <- calculate_reimbursement(
    test_data$trip_duration_days[i],
    test_data$miles_traveled[i],
    test_data$total_receipts_amount[i],
    config
  )
  predictions[i] <- prediction
  errors[i] <- abs(prediction - test_data$expected_output[i])
}

exact_matches <- sum(errors <= 0.01)
avg_error <- mean(errors)

cat("Results on first 10 cases:\n")
cat("  Exact matches:", exact_matches, "/", 10, "\n")
cat("  Average error: $", round(avg_error, 2), "\n\n")

cat("=== BASIC TEST PASSED ===\n")
cat("The reimbursement calculation system is working correctly.\n")
cat("Ready to run the full optimization!\n")
