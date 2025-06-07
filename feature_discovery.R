# Feature Discovery Script for Public Cases Data
# This script explores relationships between features and the target variable

# Load required libraries
library(ggplot2)
library(corrplot)
library(dplyr)
library(gridExtra)
library(GGally)
library(car)

# Read the data
data <- read.csv("public_cases.csv")

# Basic data exploration
cat("=== BASIC DATA EXPLORATION ===\n")
cat("Dataset dimensions:", dim(data), "\n")
cat("Column names:", colnames(data), "\n\n")

# Display first few rows
cat("First 10 rows:\n")
print(head(data, 10))

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
print(summary(data))

# Check for missing values
cat("\n=== MISSING VALUES ===\n")
missing_counts <- sapply(data, function(x) sum(is.na(x)))
print(missing_counts)

# Data types
cat("\n=== DATA TYPES ===\n")
print(str(data))

# === CORRELATION ANALYSIS ===
cat("\n=== CORRELATION ANALYSIS ===\n")
correlation_matrix <- cor(data)
print(correlation_matrix)

# Create correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8)
title("Feature Correlation Matrix")

# Also save to file
png("correlation_plot.png", width = 800, height = 600)
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8, number.cex = 0.8)
title("Feature Correlation Matrix")
dev.off()

# === DISTRIBUTION ANALYSIS ===
cat("\n=== DISTRIBUTION ANALYSIS ===\n")

# Create histograms for each variable
p1 <- ggplot(data, aes(x = trip_duration_days)) + 
  geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Trip Duration (Days)", x = "Trip Duration (Days)", y = "Frequency")

p2 <- ggplot(data, aes(x = miles_traveled)) + 
  geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7) +
  labs(title = "Distribution of Miles Traveled", x = "Miles Traveled", y = "Frequency")

p3 <- ggplot(data, aes(x = total_receipts_amount)) + 
  geom_histogram(bins = 30, fill = "salmon", alpha = 0.7) +
  labs(title = "Distribution of Total Receipts Amount", x = "Total Receipts Amount", y = "Frequency")

p4 <- ggplot(data, aes(x = expected_output)) + 
  geom_histogram(bins = 30, fill = "gold", alpha = 0.7) +
  labs(title = "Distribution of Expected Output", x = "Expected Output", y = "Frequency")

# Display distribution plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save distribution plots
png("distributions.png", width = 1200, height = 800)
grid.arrange(p1, p2, p3, p4, ncol = 2)
dev.off()

# === SCATTER PLOT ANALYSIS ===
cat("\n=== SCATTER PLOT ANALYSIS ===\n")

# Scatter plots against target variable
s1 <- ggplot(data, aes(x = trip_duration_days, y = expected_output)) + 
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Trip Duration vs Expected Output", x = "Trip Duration (Days)", y = "Expected Output")

s2 <- ggplot(data, aes(x = miles_traveled, y = expected_output)) + 
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Miles Traveled vs Expected Output", x = "Miles Traveled", y = "Expected Output")

s3 <- ggplot(data, aes(x = total_receipts_amount, y = expected_output)) + 
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Total Receipts vs Expected Output", x = "Total Receipts Amount", y = "Expected Output")

# Display scatter plots
grid.arrange(s1, s2, s3, ncol = 3)

# Save scatter plots
png("scatter_plots.png", width = 1200, height = 400)
grid.arrange(s1, s2, s3, ncol = 3)
dev.off()

# === PAIRWISE RELATIONSHIPS ===
cat("\n=== PAIRWISE RELATIONSHIPS ===\n")

# Display pairs plot
print(ggpairs(data, title = "Pairwise Relationships Between All Variables"))

# Create pairs plot
png("pairs_plot.png", width = 1000, height = 1000)
ggpairs(data, title = "Pairwise Relationships Between All Variables")
dev.off()

# === FEATURE ENGINEERING EXPLORATION ===
cat("\n=== FEATURE ENGINEERING EXPLORATION ===\n")

# Create potential new features
data_enhanced <- data %>%
  mutate(
    # Rate features
    receipts_per_day = total_receipts_amount / trip_duration_days,
    receipts_per_mile = total_receipts_amount / miles_traveled,
    miles_per_day = miles_traveled / trip_duration_days,
    
    # Interaction features
    duration_miles_interaction = trip_duration_days * miles_traveled,
    duration_receipts_interaction = trip_duration_days * total_receipts_amount,
    miles_receipts_interaction = miles_traveled * total_receipts_amount,
    
    # Polynomial features
    trip_duration_squared = trip_duration_days^2,
    miles_traveled_squared = miles_traveled^2,
    receipts_squared = total_receipts_amount^2,
    
    # Log features (adding small constant to avoid log(0))
    log_trip_duration = log(trip_duration_days + 1),
    log_miles_traveled = log(miles_traveled + 1),
    log_receipts = log(total_receipts_amount + 1)
  )

# Handle infinite values that might occur from division
data_enhanced[is.infinite(data_enhanced$receipts_per_mile), "receipts_per_mile"] <- NA

# Calculate correlations with new features
new_features <- c("receipts_per_day", "receipts_per_mile", "miles_per_day", 
                  "duration_miles_interaction", "duration_receipts_interaction", 
                  "miles_receipts_interaction", "trip_duration_squared", 
                  "miles_traveled_squared", "receipts_squared", 
                  "log_trip_duration", "log_miles_traveled", "log_receipts")

cat("Correlations of engineered features with expected_output:\n")
feature_correlations <- sapply(new_features, function(feature) {
  cor(data_enhanced[[feature]], data_enhanced$expected_output, use = "complete.obs")
})
print(sort(abs(feature_correlations), decreasing = TRUE))

# === OUTLIER ANALYSIS ===
cat("\n=== OUTLIER ANALYSIS ===\n")

# Function to identify outliers using IQR method
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

# Identify outliers for each variable
for (col in colnames(data)) {
  outliers <- identify_outliers(data[[col]])
  cat(paste("Outliers in", col, ":", length(outliers), "observations\n"))
}

# Create box plots to visualize outliers
b1 <- ggplot(data, aes(y = trip_duration_days)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Trip Duration Outliers", y = "Trip Duration (Days)")

b2 <- ggplot(data, aes(y = miles_traveled)) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(title = "Miles Traveled Outliers", y = "Miles Traveled")

b3 <- ggplot(data, aes(y = total_receipts_amount)) + 
  geom_boxplot(fill = "salmon") + 
  labs(title = "Total Receipts Outliers", y = "Total Receipts Amount")

b4 <- ggplot(data, aes(y = expected_output)) + 
  geom_boxplot(fill = "gold") + 
  labs(title = "Expected Output Outliers", y = "Expected Output")

# Display outlier analysis
grid.arrange(b1, b2, b3, b4, ncol = 4)

# Save outlier analysis
png("outlier_analysis.png", width = 1200, height = 300)
grid.arrange(b1, b2, b3, b4, ncol = 4)
dev.off()

# === SEGMENTATION ANALYSIS ===
cat("\n=== SEGMENTATION ANALYSIS ===\n")

# Analyze patterns by trip duration categories
data$duration_category <- cut(data$trip_duration_days, 
                             breaks = c(0, 2, 5, 10, Inf), 
                             labels = c("Short (1-2 days)", "Medium (3-5 days)", 
                                       "Long (6-10 days)", "Very Long (11+ days)"))

# Summary by duration category
cat("Summary statistics by trip duration category:\n")
duration_summary <- data %>%
  group_by(duration_category) %>%
  summarise(
    count = n(),
    avg_miles = mean(miles_traveled),
    avg_receipts = mean(total_receipts_amount),
    avg_output = mean(expected_output),
    .groups = 'drop'
  )
print(duration_summary)

# Create visualization by category
p_category <- ggplot(data, aes(x = duration_category, y = expected_output, fill = duration_category)) +
  geom_boxplot() +
  labs(title = "Expected Output by Trip Duration Category", 
       x = "Trip Duration Category", y = "Expected Output") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display category analysis
print(p_category)

# Save category analysis
png("category_analysis.png", width = 800, height = 600)
print(p_category)
dev.off()

# === LINEAR REGRESSION ANALYSIS ===
cat("\n=== LINEAR REGRESSION ANALYSIS ===\n")

# Fit basic linear model
model_basic <- lm(expected_output ~ trip_duration_days + miles_traveled + total_receipts_amount, data = data)
cat("Basic Linear Model Summary:\n")
print(summary(model_basic))

# Check for multicollinearity
cat("\nVariance Inflation Factors (VIF):\n")
vif_values <- vif(model_basic)
print(vif_values)

# Display residual analysis
par(mfrow = c(2, 2))
plot(model_basic)

# Save residual analysis
png("residual_analysis.png", width = 1200, height = 800)
par(mfrow = c(2, 2))
plot(model_basic)
dev.off()

# === FEATURE IMPORTANCE RANKING ===
cat("\n=== FEATURE IMPORTANCE RANKING ===\n")

# Calculate absolute correlations with target
feature_importance <- abs(cor(data[, 1:3], data$expected_output))
feature_importance_sorted <- sort(feature_importance, decreasing = TRUE)

cat("Feature importance (absolute correlation with target):\n")
print(feature_importance_sorted)

# === RECOMMENDATIONS ===
cat("\n=== FEATURE DISCOVERY RECOMMENDATIONS ===\n")
cat("1. Strong correlations found:\n")
strong_corr <- feature_importance_sorted[feature_importance_sorted > 0.5]
if(length(strong_corr) > 0) {
  for(i in 1:length(strong_corr)) {
    cat(paste("   -", names(strong_corr)[i], ":", round(strong_corr[i], 3), "\n"))
  }
} else {
  cat("   - No features with correlation > 0.5 found\n")
}

cat("\n2. Potential feature engineering opportunities:\n")
cat("   - Consider rate-based features (receipts per day, miles per day)\n")
cat("   - Interaction terms between features may be valuable\n")
cat("   - Log transformations might help with skewed distributions\n")

cat("\n3. Data quality observations:\n")
cat("   - Check for outliers in the data\n")
cat("   - Consider trip duration categories for different modeling approaches\n")
cat("   - Examine residual patterns for model improvement opportunities\n")

cat("\nFeature discovery analysis complete! Check the generated PNG files for visualizations.\n")
