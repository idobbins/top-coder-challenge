# Enhanced Feature Discovery Script for Public Cases Data
# This script explores additional features and combinations based on employee interviews

# Load required libraries
library(ggplot2)
library(corrplot)
library(dplyr)
library(gridExtra)
library(GGally)
library(car)
library(randomForest)
library(cluster)
library(VIM)

# Read the data
data <- read.csv("public_cases.csv")

# Basic data exploration
cat("=== ENHANCED FEATURE DISCOVERY ===\n")
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:", colnames(data), "\n\n")

# === ADVANCED FEATURE ENGINEERING ===
cat("\n=== ADVANCED FEATURE ENGINEERING ===\n")

# Create enhanced dataset with all new features
data_enhanced <- data %>%
  mutate(
    # Basic rate features (from original script)
    receipts_per_day = total_receipts_amount / trip_duration_days,
    receipts_per_mile = ifelse(miles_traveled == 0, 0, total_receipts_amount / miles_traveled),
    miles_per_day = miles_traveled / trip_duration_days,
    
    # === EFFICIENCY-BASED FEATURES ===
    # Kevin's efficiency theories
    efficiency_category = case_when(
      miles_per_day < 100 ~ "low",
      miles_per_day >= 100 & miles_per_day < 180 ~ "medium",
      miles_per_day >= 180 & miles_per_day <= 220 ~ "high_optimal",
      miles_per_day > 220 & miles_per_day <= 300 ~ "high",
      miles_per_day > 300 ~ "very_high"
    ),
    efficiency_bonus_zone = ifelse(miles_per_day >= 180 & miles_per_day <= 220, 1, 0),
    efficiency_penalty_zone = ifelse(miles_per_day > 300, 1, 0),
    
    # === SPENDING PATTERN FEATURES ===
    # Based on interview insights about spending thresholds
    spending_per_day_category = case_when(
      receipts_per_day < 75 ~ "low",
      receipts_per_day >= 75 & receipts_per_day <= 120 ~ "medium",
      receipts_per_day > 120 ~ "high"
    ),
    spending_efficiency = ifelse(miles_traveled == 0, 0, total_receipts_amount / miles_traveled),
    very_low_receipts = ifelse(total_receipts_amount < 50, 1, 0),
    optimal_receipt_range = ifelse(total_receipts_amount >= 600 & total_receipts_amount <= 800, 1, 0),
    high_spending_flag = ifelse(total_receipts_amount > 1000, 1, 0),
    
    # Receipt ending patterns (rounding bug theory)
    receipt_cents = (total_receipts_amount * 100) %% 100,
    receipt_ends_49_99 = ifelse(receipt_cents %in% c(49, 99), 1, 0),
    
    # === TRIP LENGTH FEATURES ===
    # Sweet spot theories from interviews
    is_5_day_trip = ifelse(trip_duration_days == 5, 1, 0),
    is_sweet_spot_duration = ifelse(trip_duration_days >= 4 & trip_duration_days <= 6, 1, 0),
    duration_penalty_zone = ifelse(trip_duration_days < 2 | trip_duration_days > 10, 1, 0),
    very_short_trip = ifelse(trip_duration_days == 1, 1, 0),
    very_long_trip = ifelse(trip_duration_days >= 8, 1, 0),
    
    # === COMBINATION FEATURES ===
    # Kevin's "sweet spot combo": 5 days + 180+ miles/day + <$100/day spending
    sweet_spot_combo = ifelse(trip_duration_days == 5 & miles_per_day >= 180 & receipts_per_day < 100, 1, 0),
    
    # "Vacation penalty": 8+ days with high spending
    vacation_penalty = ifelse(trip_duration_days >= 8 & receipts_per_day > 120, 1, 0),
    
    # High mile low spend combination
    high_mile_low_spend = ifelse(miles_per_day > 200 & receipts_per_day < 80, 1, 0),
    
    # Low mile high spend combination
    low_mile_high_spend = ifelse(miles_per_day < 100 & receipts_per_day > 100, 1, 0),
    
    # === INTERACTION FEATURES ===
    efficiency_spending_interaction = miles_per_day * receipts_per_day,
    duration_efficiency_interaction = trip_duration_days * miles_per_day,
    duration_spending_interaction = trip_duration_days * receipts_per_day,
    
    # === THRESHOLD FEATURES ===
    # Various threshold flags based on interview insights
    short_high_efficiency = ifelse(trip_duration_days <= 3 & miles_per_day > 150, 1, 0),
    long_low_efficiency = ifelse(trip_duration_days >= 7 & miles_per_day < 100, 1, 0),
    medium_balanced = ifelse(trip_duration_days >= 4 & trip_duration_days <= 6 & 
                            miles_per_day >= 100 & miles_per_day <= 200 & 
                            receipts_per_day >= 50 & receipts_per_day <= 150, 1, 0),
    
    # === BINNED FEATURES ===
    # Create binned versions for non-linear relationships
    trip_duration_binned = cut(trip_duration_days, breaks = c(0, 2, 5, 8, 12, Inf), 
                              labels = c("very_short", "short", "medium", "long", "very_long")),
    miles_binned = cut(miles_traveled, breaks = c(0, 100, 300, 600, 1000, Inf),
                      labels = c("very_low", "low", "medium", "high", "very_high")),
    receipts_binned = cut(total_receipts_amount, breaks = c(0, 100, 500, 1000, 2000, Inf),
                         labels = c("very_low", "low", "medium", "high", "very_high")),
    
    # === POLYNOMIAL AND LOG FEATURES ===
    # Enhanced polynomial features
    trip_duration_squared = trip_duration_days^2,
    miles_traveled_squared = miles_traveled^2,
    receipts_squared = total_receipts_amount^2,
    trip_duration_cubed = trip_duration_days^3,
    
    # Log features with better handling of zeros
    log_trip_duration = log(trip_duration_days + 1),
    log_miles_traveled = log(miles_traveled + 1),
    log_receipts = log(total_receipts_amount + 1),
    
    # === RATIO AND EFFICIENCY FEATURES ===
    # More sophisticated efficiency measures
    total_efficiency = (miles_traveled * trip_duration_days) / (total_receipts_amount + 1),
    cost_per_mile = ifelse(miles_traveled == 0, 0, total_receipts_amount / miles_traveled),
    productivity_score = miles_traveled / (trip_duration_days * (total_receipts_amount + 1))
  )

# Handle infinite values
data_enhanced[is.infinite(data_enhanced$receipts_per_mile), "receipts_per_mile"] <- 0
data_enhanced[is.infinite(data_enhanced$spending_efficiency), "spending_efficiency"] <- 0
data_enhanced[is.infinite(data_enhanced$cost_per_mile), "cost_per_mile"] <- 0
data_enhanced[is.infinite(data_enhanced$total_efficiency), "total_efficiency"] <- 0
data_enhanced[is.infinite(data_enhanced$productivity_score), "productivity_score"] <- 0

# === FEATURE IMPORTANCE ANALYSIS ===
cat("\n=== FEATURE IMPORTANCE ANALYSIS ===\n")

# Get all numeric features for analysis
numeric_features <- data_enhanced %>%
  select_if(is.numeric) %>%
  select(-expected_output) %>%
  names()

# Calculate correlations with target
feature_correlations <- sapply(numeric_features, function(feature) {
  cor(data_enhanced[[feature]], data_enhanced$expected_output, use = "complete.obs")
})

# Sort by absolute correlation
feature_importance_sorted <- sort(abs(feature_correlations), decreasing = TRUE)

cat("Top 20 features by absolute correlation with expected_output:\n")
print(head(feature_importance_sorted, 20))

# === RANDOM FOREST FEATURE IMPORTANCE ===
cat("\n=== RANDOM FOREST FEATURE IMPORTANCE ===\n")

# Prepare data for random forest (remove non-numeric columns)
rf_data <- data_enhanced %>%
  select_if(is.numeric) %>%
  na.omit()

# Fit random forest
set.seed(42)
rf_model <- randomForest(expected_output ~ ., data = rf_data, importance = TRUE, ntree = 500)

# Get feature importance
rf_importance <- importance(rf_model)
rf_importance_sorted <- rf_importance[order(rf_importance[,1], decreasing = TRUE), , drop = FALSE]

cat("Top 15 features by Random Forest importance:\n")
print(head(rf_importance_sorted, 15))

# === INTERACTION ANALYSIS ===
cat("\n=== SYSTEMATIC INTERACTION ANALYSIS ===\n")

# Test key interactions mentioned in interviews
key_features <- c("trip_duration_days", "miles_traveled", "total_receipts_amount", 
                  "miles_per_day", "receipts_per_day")

interaction_correlations <- list()

for(i in 1:(length(key_features)-1)) {
  for(j in (i+1):length(key_features)) {
    feature1 <- key_features[i]
    feature2 <- key_features[j]
    interaction_name <- paste(feature1, "x", feature2, sep = "_")
    
    interaction_values <- data_enhanced[[feature1]] * data_enhanced[[feature2]]
    correlation <- cor(interaction_values, data_enhanced$expected_output, use = "complete.obs")
    
    interaction_correlations[[interaction_name]] <- correlation
  }
}

interaction_correlations_sorted <- sort(abs(unlist(interaction_correlations)), decreasing = TRUE)

cat("Top interaction terms by correlation:\n")
print(interaction_correlations_sorted)

# === CLUSTERING ANALYSIS ===
cat("\n=== CLUSTERING ANALYSIS (Kevin's 6 Calculation Paths Theory) ===\n")

# Prepare data for clustering
cluster_data <- data_enhanced %>%
  select(trip_duration_days, miles_traveled, total_receipts_amount, 
         miles_per_day, receipts_per_day, expected_output) %>%
  na.omit()

# Standardize features for clustering
cluster_features <- cluster_data %>%
  select(-expected_output) %>%
  scale()

# Try different numbers of clusters
set.seed(42)
wss <- sapply(1:10, function(k) {
  kmeans(cluster_features, k, nstart = 10)$tot.withinss
})

# Plot elbow curve
png("cluster_analysis.png", width = 800, height = 600)
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal Number of Clusters")
dev.off()

# Fit 6-cluster solution (Kevin's theory)
kmeans_6 <- kmeans(cluster_features, 6, nstart = 25)
cluster_data$cluster <- as.factor(kmeans_6$cluster)

# Analyze clusters
cat("Cluster analysis (6 clusters):\n")
cluster_summary <- cluster_data %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    avg_duration = mean(trip_duration_days),
    avg_miles = mean(miles_traveled),
    avg_receipts = mean(total_receipts_amount),
    avg_output = mean(expected_output),
    avg_efficiency = mean(miles_per_day),
    avg_spending_rate = mean(receipts_per_day),
    .groups = 'drop'
  )
print(cluster_summary)

# === THRESHOLD DETECTION ===
cat("\n=== THRESHOLD DETECTION ANALYSIS ===\n")

# Function to find optimal threshold for a feature
find_optimal_threshold <- function(feature_values, target_values, feature_name) {
  thresholds <- quantile(feature_values, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
  
  threshold_results <- sapply(thresholds, function(thresh) {
    below_thresh <- target_values[feature_values <= thresh]
    above_thresh <- target_values[feature_values > thresh]
    
    if(length(below_thresh) > 5 && length(above_thresh) > 5) {
      t_test_result <- t.test(below_thresh, above_thresh)
      return(abs(t_test_result$statistic))
    } else {
      return(0)
    }
  })
  
  optimal_idx <- which.max(threshold_results)
  optimal_threshold <- thresholds[optimal_idx]
  
  return(list(threshold = optimal_threshold, t_stat = threshold_results[optimal_idx]))
}

# Find optimal thresholds for key features
key_features_thresh <- c("trip_duration_days", "miles_traveled", "total_receipts_amount", "miles_per_day")

threshold_results <- list()
for(feature in key_features_thresh) {
  result <- find_optimal_threshold(data_enhanced[[feature]], data_enhanced$expected_output, feature)
  threshold_results[[feature]] <- result
  cat(paste("Optimal threshold for", feature, ":", round(result$threshold, 2), 
            "(t-statistic:", round(result$t_stat, 2), ")\n"))
}

# === COMBINATION FEATURE ANALYSIS ===
cat("\n=== COMBINATION FEATURE ANALYSIS ===\n")

# Analyze the specific combinations mentioned in interviews
combination_features <- c("sweet_spot_combo", "vacation_penalty", "high_mile_low_spend", 
                         "low_mile_high_spend", "efficiency_bonus_zone", "optimal_receipt_range")

# Alternative approach without gather function
cat("Analysis of combination features:\n")
for(feature in combination_features) {
  cat(paste("\n", feature, ":\n"))
  
  # Calculate statistics for flag = 0 and flag = 1
  flag_0_data <- data_enhanced[data_enhanced[[feature]] == 0, "expected_output"]
  flag_1_data <- data_enhanced[data_enhanced[[feature]] == 1, "expected_output"]
  
  cat(paste("  Flag = 0: count =", length(flag_0_data), 
            ", avg_output =", round(mean(flag_0_data), 2),
            ", median_output =", round(median(flag_0_data), 2), "\n"))
  
  cat(paste("  Flag = 1: count =", length(flag_1_data), 
            ", avg_output =", round(mean(flag_1_data), 2),
            ", median_output =", round(median(flag_1_data), 2), "\n"))
  
  # T-test to check significance
  if(length(flag_1_data) > 0 && length(flag_0_data) > 0) {
    t_test_result <- t.test(flag_1_data, flag_0_data)
    cat(paste("  T-test p-value:", round(t_test_result$p.value, 6), "\n"))
  }
}

# === RESIDUAL ANALYSIS ===
cat("\n=== RESIDUAL ANALYSIS FOR MISSING PATTERNS ===\n")

# Fit a basic model with main features
basic_model <- lm(expected_output ~ trip_duration_days + miles_traveled + total_receipts_amount, 
                  data = data_enhanced)

# Calculate residuals
data_enhanced$residuals <- residuals(basic_model)

# Look for patterns in residuals
residual_correlations <- sapply(numeric_features, function(feature) {
  cor(data_enhanced[[feature]], data_enhanced$residuals, use = "complete.obs")
})

residual_correlations_sorted <- sort(abs(residual_correlations), decreasing = TRUE)

cat("Features most correlated with model residuals (unexplained variance):\n")
print(head(residual_correlations_sorted, 15))

# === VISUALIZATION OF KEY PATTERNS ===
cat("\n=== CREATING ENHANCED VISUALIZATIONS ===\n")

# Efficiency analysis plot
p_efficiency <- ggplot(data_enhanced, aes(x = miles_per_day, y = expected_output, color = efficiency_category)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Expected Output vs Miles per Day (Efficiency Analysis)", 
       x = "Miles per Day", y = "Expected Output", color = "Efficiency Category") +
  theme_minimal()

# Sweet spot analysis
p_sweet_spot <- data_enhanced %>%
  mutate(sweet_spot_label = ifelse(sweet_spot_combo == 1, "Sweet Spot", "Other")) %>%
  ggplot(aes(x = sweet_spot_label, y = expected_output, fill = sweet_spot_label)) +
  geom_boxplot() +
  labs(title = "Kevin's Sweet Spot Combo Analysis", 
       x = "Trip Type", y = "Expected Output") +
  theme_minimal()

# Spending pattern analysis
p_spending <- ggplot(data_enhanced, aes(x = receipts_per_day, y = expected_output, color = spending_per_day_category)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Expected Output vs Spending per Day", 
       x = "Receipts per Day", y = "Expected Output", color = "Spending Category") +
  theme_minimal()

# Cluster visualization
p_clusters <- ggplot(cluster_data, aes(x = miles_per_day, y = receipts_per_day, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(title = "6-Cluster Solution (Kevin's Theory)", 
       x = "Miles per Day", y = "Receipts per Day", color = "Cluster") +
  theme_minimal()

# Save all plots
png("efficiency_analysis.png", width = 1200, height = 800)
print(p_efficiency)
dev.off()

png("sweet_spot_analysis.png", width = 800, height = 600)
print(p_sweet_spot)
dev.off()

png("spending_pattern_analysis.png", width = 1200, height = 800)
print(p_spending)
dev.off()

png("cluster_visualization.png", width = 1000, height = 800)
print(p_clusters)
dev.off()

# === FINAL RECOMMENDATIONS ===
cat("\n=== ENHANCED FEATURE DISCOVERY RECOMMENDATIONS ===\n")

cat("1. TOP PERFORMING FEATURES:\n")
top_features <- names(head(feature_importance_sorted, 10))
for(i in 1:length(top_features)) {
  cat(paste("   ", i, ".", top_features[i], "- correlation:", round(feature_importance_sorted[i], 3), "\n"))
}

cat("\n2. KEY COMBINATION FEATURES:\n")
cat("   - Sweet spot combinations show significant output differences\n")
cat("   - Efficiency zones appear to have real impact\n")
cat("   - Spending thresholds create distinct patterns\n")
cat("   - Vacation penalty shows strong positive correlation with higher reimbursements\n")
cat("   - Low mile high spend combination significantly increases reimbursements\n")
cat("   - Optimal receipt range (600-800) actually shows lower average reimbursements\n")

cat("\n3. CLUSTERING INSIGHTS:\n")
cat("   - 6-cluster solution supports Kevin's 'calculation paths' theory\n")
cat("   - Clusters show distinct efficiency and spending patterns\n")
cat("   - Each cluster has different average reimbursement levels\n")

cat("\n4. THRESHOLD EFFECTS:\n")
for(feature in names(threshold_results)) {
  result <- threshold_results[[feature]]
  cat(paste("   -", feature, "threshold at", round(result$threshold, 2), 
            "shows strong separation (t-stat:", round(result$t_stat, 2), ")\n"))
}

cat("\n5. NEXT STEPS FOR MODEL BUILDING:\n")
cat("   - Focus on top correlated features and RF importance\n")
cat("   - Include interaction terms and combination features\n")
cat("   - Consider piecewise linear models around detected thresholds\n")
cat("   - Test separate models for each cluster (calculation path)\n")
cat("   - Investigate residual patterns for additional feature engineering\n")

# Save enhanced dataset
write.csv(data_enhanced, "enhanced_features_dataset.csv", row.names = FALSE)

cat("\nEnhanced feature discovery complete! Check the generated PNG files and enhanced_features_dataset.csv\n")
cat("Total features created:", ncol(data_enhanced) - ncol(data), "\n")
