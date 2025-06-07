#!/usr/bin/env python3
"""
Legacy Reimbursement System Replica
Reverse-engineered from 1000 historical cases and employee interviews

This implementation preserves all bugs and quirks from the original 60-year-old system,
including the critical rounding bug and 6 different calculation paths.
"""

import sys
import math
import json

class ModelConfig:
    def __init__(self):
        # Base calculation parameters (R-OPTIMIZED)
        self.base_per_diem = 45.51959
        self.mileage_rates = [0.78707, 0.45114, 0.20083]
        self.receipt_rates = [0.39998, 0.30454, 0.43686, 0.12188]
        
        # Cluster adjustment multipliers (R-OPTIMIZED)
        self.cluster_multipliers = [1.14757, 1.11176, 1.20275, 1.19116, 1.07688, 1.21401]
        self.vacation_bonus_multiplier = 1.04678
        self.vacation_base_multiplier = 1.07235
        
        # Threshold effect multipliers (R-OPTIMIZED)
        self.five_day_multiplier = 1.08250
        self.receipt_threshold_multiplier = 1.19667
        self.efficiency_threshold_multiplier = 1.01898
        self.miles_threshold_multiplier = 1.02133
        
        # Interaction coefficients (R-OPTIMIZED)
        self.interaction_coeff = 0.00055520
        self.log_bonus_coeff = 5.12776
        
        # Rounding bug parameters (R-OPTIMIZED)
        self.bug_base_bonus = 20.69937
        self.bug_multiplier = 0.00617


class LegacyReimbursementSystem:
    def __init__(self):
        self.is_trained = False
        self.cluster_models = {}
        self.kmeans_model = None
        self.feature_scaler = None
        self.config = ModelConfig()
        
    def engineer_features(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Engineer all 42 features discovered in our enhanced analysis
        """
        # Basic rate features
        receipts_per_day = total_receipts_amount / trip_duration_days
        receipts_per_mile = total_receipts_amount / miles_traveled if miles_traveled > 0 else 0
        miles_per_day = miles_traveled / trip_duration_days
        
        # Efficiency-based features (Kevin's theories)
        if miles_per_day < 100:
            efficiency_category = 0  # low
        elif miles_per_day < 180:
            efficiency_category = 1  # medium
        elif miles_per_day <= 220:
            efficiency_category = 2  # high_optimal
        elif miles_per_day <= 300:
            efficiency_category = 3  # high
        else:
            efficiency_category = 4  # very_high
            
        efficiency_bonus_zone = 1 if 180 <= miles_per_day <= 220 else 0
        efficiency_penalty_zone = 1 if miles_per_day > 300 else 0
        
        # Spending pattern features
        if receipts_per_day < 75:
            spending_category = 0  # low
        elif receipts_per_day <= 120:
            spending_category = 1  # medium
        else:
            spending_category = 2  # high
            
        very_low_receipts = 1 if total_receipts_amount < 50 else 0
        optimal_receipt_range = 1 if 600 <= total_receipts_amount <= 800 else 0
        high_spending_flag = 1 if total_receipts_amount > 1000 else 0
        
        # CRITICAL: The rounding bug (highest Random Forest importance!)
        receipt_cents = (total_receipts_amount * 100) % 100
        receipt_ends_49_99 = 1 if receipt_cents in [49, 99] else 0
        
        # Trip length features
        is_5_day_trip = 1 if trip_duration_days == 5 else 0
        is_sweet_spot_duration = 1 if 4 <= trip_duration_days <= 6 else 0
        duration_penalty_zone = 1 if trip_duration_days < 2 or trip_duration_days > 10 else 0
        very_short_trip = 1 if trip_duration_days == 1 else 0
        very_long_trip = 1 if trip_duration_days >= 8 else 0
        
        # Combination features
        sweet_spot_combo = 1 if (trip_duration_days == 5 and miles_per_day >= 180 and receipts_per_day < 100) else 0
        vacation_penalty = 1 if (trip_duration_days >= 8 and receipts_per_day > 120) else 0  # Actually a bonus!
        high_mile_low_spend = 1 if (miles_per_day > 200 and receipts_per_day < 80) else 0
        low_mile_high_spend = 1 if (miles_per_day < 100 and receipts_per_day > 100) else 0
        
        # Interaction features
        efficiency_spending_interaction = miles_per_day * receipts_per_day
        duration_efficiency_interaction = trip_duration_days * miles_per_day
        duration_spending_interaction = trip_duration_days * receipts_per_day
        
        # Threshold features
        short_high_efficiency = 1 if (trip_duration_days <= 3 and miles_per_day > 150) else 0
        long_low_efficiency = 1 if (trip_duration_days >= 7 and miles_per_day < 100) else 0
        medium_balanced = 1 if (4 <= trip_duration_days <= 6 and 100 <= miles_per_day <= 200 and 50 <= receipts_per_day <= 150) else 0
        
        # Mathematical transformations
        log_trip_duration = math.log(trip_duration_days + 1)
        log_miles_traveled = math.log(miles_traveled + 1)
        log_receipts = math.log(total_receipts_amount + 1)
        
        trip_duration_squared = trip_duration_days ** 2
        miles_traveled_squared = miles_traveled ** 2
        receipts_squared = total_receipts_amount ** 2
        trip_duration_cubed = trip_duration_days ** 3
        
        # Ratio and efficiency features
        total_efficiency = (miles_traveled * trip_duration_days) / (total_receipts_amount + 1)
        cost_per_mile = total_receipts_amount / miles_traveled if miles_traveled > 0 else 0
        productivity_score = miles_traveled / (trip_duration_days * (total_receipts_amount + 1))
        
        # Return feature vector in the same order as training data
        features = [
            trip_duration_days, miles_traveled, total_receipts_amount,
            receipts_per_day, receipts_per_mile, miles_per_day,
            efficiency_bonus_zone, efficiency_penalty_zone,
            very_low_receipts, optimal_receipt_range, high_spending_flag,
            receipt_ends_49_99,  # CRITICAL ROUNDING BUG
            is_5_day_trip, is_sweet_spot_duration, duration_penalty_zone,
            very_short_trip, very_long_trip,
            sweet_spot_combo, vacation_penalty, high_mile_low_spend, low_mile_high_spend,
            efficiency_spending_interaction, duration_efficiency_interaction, duration_spending_interaction,
            short_high_efficiency, long_low_efficiency, medium_balanced,
            trip_duration_squared, miles_traveled_squared, receipts_squared, trip_duration_cubed,
            log_trip_duration, log_miles_traveled, log_receipts,
            total_efficiency, cost_per_mile, productivity_score
        ]
        
        return [features]
    
    def determine_cluster(self, features):
        """
        Determine which of the 6 calculation paths to use based on Kevin's theory
        """
        if not self.is_trained:
            # Fallback cluster determination based on our analysis
            trip_duration = features[0][0]
            miles_traveled = features[0][1] 
            receipts = features[0][2]
            miles_per_day = features[0][5]
            receipts_per_day = features[0][3]
            
            # Cluster logic based on our analysis:
            if trip_duration <= 1.5 and miles_per_day > 500:  # Cluster 1: Very short, extremely high efficiency
                return 0
            elif trip_duration > 8 and miles_per_day < 50:    # Cluster 2: Long, very low efficiency  
                return 1
            elif 3 <= trip_duration <= 5 and receipts > 1500: # Cluster 3: Medium trips, very high spending
                return 2
            elif 4 <= trip_duration <= 7 and miles_per_day > 150 and receipts < 800: # Cluster 4: Medium, high efficiency, low spending
                return 3
            elif trip_duration > 8 and receipts > 1200:       # Cluster 5: Long trips, high spending
                return 4
            else:                                              # Cluster 6: Everything else (short, low efficiency/spending)
                return 5
        else:
            # Use trained clustering model
            cluster_features = features[:, [0, 1, 2, 5, 3]]  # duration, miles, receipts, miles_per_day, receipts_per_day
            return self.kmeans_model.predict(cluster_features)[0]
    
    def apply_rounding_bug(self, amount, receipt_ends_49_99):
        """
        Apply the critical rounding bug discovered in our analysis
        This was the highest importance feature in Random Forest (27.03% IncMSE)
        """
        if receipt_ends_49_99:
            # The bug: system rounds up twice for receipts ending in .49 or .99
            bug_bonus = self.config.bug_base_bonus + (amount * self.config.bug_multiplier)
            return amount + bug_bonus
        return amount
    
    def calculate_base_reimbursement(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Calculate base reimbursement using the patterns we discovered
        """
        # Base per diem using configurable parameter
        base_per_diem = self.config.base_per_diem * trip_duration_days
        
        # Mileage calculation with configurable rates
        if miles_traveled <= 100:
            mileage_reimbursement = miles_traveled * self.config.mileage_rates[0]
        elif miles_traveled <= 500:
            mileage_reimbursement = 100 * self.config.mileage_rates[0] + (miles_traveled - 100) * self.config.mileage_rates[1]
        else:
            mileage_reimbursement = 100 * self.config.mileage_rates[0] + 400 * self.config.mileage_rates[1] + (miles_traveled - 500) * self.config.mileage_rates[2]
        
        # Receipt reimbursement with configurable rates
        if total_receipts_amount <= 50:
            receipt_reimbursement = total_receipts_amount * self.config.receipt_rates[0]
        elif total_receipts_amount <= 500:
            receipt_reimbursement = 15 + (total_receipts_amount - 50) * self.config.receipt_rates[1]
        elif total_receipts_amount <= 1500:
            receipt_reimbursement = 15 + 180 + (total_receipts_amount - 500) * self.config.receipt_rates[2]
        else:
            receipt_reimbursement = 15 + 180 + 300 + (total_receipts_amount - 1500) * self.config.receipt_rates[3]
        
        return base_per_diem + mileage_reimbursement + receipt_reimbursement
    
    def apply_cluster_adjustments(self, base_amount, cluster, features):
        """
        Apply cluster-specific adjustments based on the 6 calculation paths
        """
        vacation_penalty = features[0][18]  # Actually a bonus
        
        if cluster == 4 and vacation_penalty:  # Long trips, high spending with vacation bonus
            return base_amount * self.config.vacation_bonus_multiplier
        elif cluster == 4:  # Long trips, high spending without vacation bonus
            return base_amount * self.config.vacation_base_multiplier
        else:
            return base_amount * self.config.cluster_multipliers[cluster]
    
    def apply_threshold_effects(self, amount, features):
        """
        Apply threshold effects we discovered in the analysis
        """
        trip_duration = features[0][0]
        miles_traveled = features[0][1]
        total_receipts = features[0][2]
        miles_per_day = features[0][5]
        is_5_day_trip = features[0][12]
        
        # 5-day sweet spot (strongest threshold effect, t-stat: 16.05)
        if is_5_day_trip:
            amount *= self.config.five_day_multiplier
        
        # Receipt amount threshold at $660.54 (t-stat: 33.05)
        if total_receipts > 660.54:
            amount *= self.config.receipt_threshold_multiplier
        
        # Miles per day efficiency threshold at 187.01 (t-stat: 7.44)
        if miles_per_day > 187.01:
            amount *= self.config.efficiency_threshold_multiplier
        
        # Miles traveled threshold at 473.8 (t-stat: 13.42)
        if miles_traveled > 473.8:
            amount *= self.config.miles_threshold_multiplier
        
        return amount
    
    def calculate_reimbursement(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Main calculation method that replicates the legacy system
        """
        # Engineer all features
        features = self.engineer_features(trip_duration_days, miles_traveled, total_receipts_amount)
        
        # Determine calculation path (cluster)
        cluster = self.determine_cluster(features)
        
        # Calculate base reimbursement
        base_amount = self.calculate_base_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount)
        
        # Apply cluster-specific adjustments
        adjusted_amount = self.apply_cluster_adjustments(base_amount, cluster, features)
        
        # Apply threshold effects
        threshold_adjusted = self.apply_threshold_effects(adjusted_amount, features)
        
        # Apply the critical rounding bug
        receipt_ends_49_99 = features[0][11]
        final_amount = self.apply_rounding_bug(threshold_adjusted, receipt_ends_49_99)
        
        # Apply interaction effects (top correlations from our analysis) - configurable
        duration_spending_interaction = features[0][23]
        log_receipts = features[0][32]
        
        # Duration × spending interaction (0.704 correlation)
        interaction_bonus = duration_spending_interaction * self.config.interaction_coeff
        
        # Log receipts effect (0.718 correlation - highest!)
        log_bonus = log_receipts * self.config.log_bonus_coeff
        
        final_amount += interaction_bonus + log_bonus
        
        # Ensure reasonable bounds and round to 2 decimal places
        final_amount = max(50.0, min(final_amount, 5000.0))
        return round(final_amount, 2)

def main():
    if len(sys.argv) != 4:
        print("Usage: python3 calculate_reimbursement.py <trip_duration_days> <miles_traveled> <total_receipts_amount>")
        sys.exit(1)
    
    try:
        trip_duration_days = int(sys.argv[1])
        miles_traveled = float(sys.argv[2])
        total_receipts_amount = float(sys.argv[3])
        
        # Validate inputs
        if trip_duration_days <= 0 or miles_traveled < 0 or total_receipts_amount < 0:
            raise ValueError("Invalid input values")
        
        # Create and use the legacy system replica
        system = LegacyReimbursementSystem()
        reimbursement = system.calculate_reimbursement(trip_duration_days, miles_traveled, total_receipts_amount)
        
        # Output just the number as required
        print(f"{reimbursement:.2f}")
        
    except (ValueError, IndexError) as e:
        print(f"Error: Invalid input parameters", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error: {str(e)}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
