#!/usr/bin/env Rscript

# Black Box Challenge - Results Generation Script (R Version)
# This script runs your R implementation against test cases and outputs results to private_results.txt

# Load required libraries
suppressMessages({
  library(jsonlite)
})

# Source the calculation functions
if (!file.exists("reimbursement_functions.R")) {
  cat("❌ Error: reimbursement_functions.R not found!\n")
  cat("Please ensure the reimbursement functions file is in the current directory.\n")
  quit(status = 1)
}

source("reimbursement_functions.R")

cat("🧾 Black Box Challenge - Generating Private Results (R Version)\n")
cat("===============================================================\n")
cat("\n")

# Check if private cases exist
if (!file.exists("private_cases.json")) {
  cat("❌ Error: private_cases.json not found!\n")
  cat("Please ensure the private cases file is in the current directory.\n")
  quit(status = 1)
}

cat("📊 Processing test cases and generating results...\n")
cat("📝 Output will be saved to private_results.txt\n")
cat("\n")

# Load test data
cat("Loading test data from private_cases.json...\n")
tryCatch({
  test_data <- jsonlite::fromJSON("private_cases.json")
}, error = function(e) {
  cat("❌ Error loading private_cases.json:", e$message, "\n")
  quit(status = 1)
})

# Debug: Show how many records were loaded
cat("Loaded", length(test_data), "records from JSON\n")

# Convert to data frame format for easier processing
if (is.list(test_data) && !is.data.frame(test_data)) {
  # Handle case where test_data is a list of cases
  test_cases <- data.frame(
    trip_duration_days = sapply(test_data, function(x) x$trip_duration_days),
    miles_traveled = sapply(test_data, function(x) x$miles_traveled),
    total_receipts_amount = sapply(test_data, function(x) x$total_receipts_amount)
  )
} else {
  # Handle case where test_data is already structured
  test_cases <- data.frame(
    trip_duration_days = test_data$trip_duration_days,
    miles_traveled = test_data$miles_traveled,
    total_receipts_amount = test_data$total_receipts_amount
  )
}

# Debug: Show how many rows in data frame
cat("Created data frame with", nrow(test_cases), "rows\n")

# Ensure we only process exactly 5000 cases
if (nrow(test_cases) > 5000) {
  cat("⚠️  Warning: Found", nrow(test_cases), "cases, truncating to 5000\n")
  test_cases <- test_cases[1:5000, ]
} else if (nrow(test_cases) < 5000) {
  cat("❌ Error: Only found", nrow(test_cases), "cases, expected 5000\n")
  quit(status = 1)
}

total_cases <- 5000  # Force to exactly 5000
cat("Processing", total_cases, "test cases...\n")

# Remove existing results file if it exists
if (file.exists("private_results.txt")) {
  file.remove("private_results.txt")
}

# Get the config once for efficiency
config <- get_config()

# Initialize results vector - ensure exactly 5000 elements
results <- character(5000)
error_count <- 0

# Process each test case
start_time <- Sys.time()

for (i in 1:5000) {
  # Progress indicator
  if (i %% 100 == 0 && i > 0) {
    cat("Progress:", i, "/", total_cases, "cases processed...\n")
  }
  
  # Extract test case data
  trip_duration <- test_cases$trip_duration_days[i]
  miles_traveled <- test_cases$miles_traveled[i]
  receipts_amount <- test_cases$total_receipts_amount[i]
  
  # Calculate reimbursement
  tryCatch({
    result <- calculate_reimbursement(
      trip_duration,
      miles_traveled,
      receipts_amount,
      config
    )
    
    # Ensure result is a valid number
    if (is.numeric(result) && !is.na(result) && is.finite(result)) {
      results[i] <- as.character(result)
    } else {
      cat("Error on case", i, ": Invalid calculation result:", result, "\n")
      results[i] <- "ERROR"
      error_count <- error_count + 1
    }
    
  }, error = function(e) {
    cat("Error on case", i, ": Calculation failed:", e$message, "\n")
    results[i] <- "ERROR"
    error_count <- error_count + 1
  })
}

end_time <- Sys.time()
runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Write results to file - ensure exactly 5000 lines
cat("\nWriting results to private_results.txt...\n")
tryCatch({
  # Ensure exactly 5000 results
  results <- results[1:5000]  # Force exactly 5000 elements
  
  # Write to file using writeLines which handles line endings properly
  writeLines(results, "private_results.txt")
  
}, error = function(e) {
  cat("❌ Error writing results file:", e$message, "\n")
  quit(status = 1)
})

# Verify the output file has exactly 5000 lines using system command
system_line_count <- as.numeric(system("wc -l < private_results.txt", intern = TRUE))
cat("✅ Output file contains", system_line_count, "lines (via wc -l)\n")

# Also verify with grep to count actual data lines
data_line_count <- as.numeric(system("grep -c '^[0-9]' private_results.txt", intern = TRUE))
cat("✅ Output file contains", data_line_count, "data lines (via grep)\n")

if (system_line_count != 5000) {
  cat("❌ Error: Output file should have exactly 5000 lines!\n")
  cat("Attempting to fix by truncating to exactly 5000 lines...\n")
  system("head -n 5000 private_results.txt > temp_results.txt && mv temp_results.txt private_results.txt")
  
  # Re-verify
  final_line_count <- as.numeric(system("wc -l < private_results.txt", intern = TRUE))
  cat("✅ After fix: Output file contains", final_line_count, "lines\n")
  
  if (final_line_count != 5000) {
    cat("❌ Error: Still unable to create exactly 5000 lines!\n")
    quit(status = 1)
  }
}

cat("\n")
cat("✅ Results generated successfully!\n")
cat("📄 Output saved to private_results.txt\n")
cat("📊 Each line contains the result for the corresponding test case in private_cases.json\n")
cat("⏱️  Processing time:", round(runtime, 2), "seconds\n")

if (error_count > 0) {
  cat("⚠️  Errors encountered:", error_count, "out of", total_cases, "cases\n")
  cat("   Lines with 'ERROR' indicate cases where calculation failed\n")
} else {
  cat("🎯 All cases processed successfully!\n")
}

cat("\n")
cat("🎯 Next steps:\n")
cat("  1. Check private_results.txt - it should contain one result per line\n")
cat("  2. Each line corresponds to the same-numbered test case in private_cases.json\n")
if (error_count > 0) {
  cat("  3. Lines with 'ERROR' indicate cases where your calculation failed\n")
  cat("  4. Review error messages above to debug any issues\n")
  cat("  5. Submit your private_results.txt file when ready!\n")
} else {
  cat("  3. Submit your private_results.txt file when ready!\n")
}
cat("\n")
cat("📈 File format:\n")
cat("  Line 1: Result for private_cases.json[0]\n")
cat("  Line 2: Result for private_cases.json[1]\n")
cat("  Line 3: Result for private_cases.json[2]\n")
cat("  ...\n")
cat("  Line N: Result for private_cases.json[N-1]\n")
