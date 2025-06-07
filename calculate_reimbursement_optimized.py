#!/usr/bin/env python3
"""
Optimized Legacy Reimbursement System Replica
Based on enhanced feature analysis and R optimization results

Key improvements:
1. Fixed rounding bug implementation
2. Enhanced feature engineering from R analysis
3. Better cluster determination logic
4. Improved parameter precision
"""

import sys
import math
import json

class ModelConfig:
    def __init__(self):
        # Base calculation parameters (R-OPTIMIZED with full precision)
        self.base_per_diem = 45.5195880490612
        self.mileage_rates = [0.787071175593883, 0.45114227601327, 0.200831651920261]
        self.receipt_rates = [0.399975746217208, 0.3045420665708, 0.436856841815577, 0.121882355942393]
        
        # Cluster adjustment multipliers (R-OPTIMIZED with full precision)
        self.cluster_multipliers = [1.14756663634639, 1.11176444876928, 1.20275399349877, 
                                   1.19116480573546, 1.07688362098627, 1.2140054738624]
        self.vacation_bonus_multiplier = 1.04678047616035
        self.vacation_base_multiplier = 1.07234985943884
        
        # Threshold effect multipliers (R-OPTIMIZED with full precision)
        self.five_day_multiplier = 1.08250167328268
        self.receipt_threshold_multiplier = 1.19666525936238
        self.efficiency_threshold_multiplier = 1.01898035206799
        self.miles_threshold_multiplier = 1.02133465968143
        
        # Interaction coefficients (R-OPTIMIZED with full precision)
        self.interaction_coeff = 0.000555204802994334
        self.log_bonus_coeff = 5.12776389976777
        
        # Rounding bug parameters (R-OPTIMIZED with full precision) - FIXED!
        self.bug_base_bonus = 20.6993690179661
        self.bug_multiplier = 0.00616844543255866


class LegacyReimbursementSystem:
    def __init__(self):
        self.config = ModelConfig()
        
    def engineer_features(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Engineer all features based on enhanced R analysis
        """
        # Basic rate features
        receipts_per_day = total_receipts_amount / trip_duration_days
        receipts_per_mile = total_receipts_amount / miles_traveled if miles_traveled > 0 else 0
        miles_per_day = miles_traveled / trip_duration_days
        
        # Efficiency-based features (from R analysis)
        efficiency_bonus_zone = 1 if 180 <= miles_per_day <= 220 else 0
        efficiency_penalty_zone = 1 if miles_per_day > 300 else 0
        
        # Spending pattern features (from R analysis)
        very_low_receipts = 1 if total_receipts_amount < 50 else 0
        optimal_receipt_range = 1 if 600 <= total_receipts_amount <= 800 else 0
        high_spending_flag = 1 if total_receipts_amount > 1000 else 0
        
        # CRITICAL: The rounding bug (FIXED implementation)
        # Use more precise calculation to avoid floating point issues
        receipt_cents = round((total_receipts_amount * 100) % 100)
        receipt_ends_49_99 = 1 if receipt_cents in [49, 99] else 0
        
        # Trip length features
        is_5_day_trip = 1 if trip_duration_days == 5 else 0
        is_sweet_spot_duration = 1 if 4 <= trip_duration_days <= 6 else 0
        duration_penalty_zone = 1 if trip_duration_days < 2 or trip_duration_days > 10 else 0
        very_short_trip = 1 if trip_duration_days == 1 else 0
        very_long_trip = 1 if trip_duration_days >= 8 else 0
        
        # Combination features (from enhanced R analysis)
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
        
        # Return feature vector
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
        Enhanced cluster determination based on R analysis patterns
        """
        trip_duration = features[0][0]
        miles_traveled = features[0][1] 
        receipts = features[0][2]
        miles_per_day = features[0][5]
        receipts_per_day = features[0][3]
        
        # Enhanced cluster logic based on R analysis results:
        # Cluster 1: Very short trips (1.1 days avg) with extremely high efficiency (785 miles/day avg)
        if trip_duration <= 1.5 and miles_per_day > 500:
            return 0
        
        # Cluster 2: Long trips (10.8 days avg) with very low efficiency (26.3 miles/day avg)  
        elif trip_duration > 8 and miles_per_day < 50:
            return 1
        
        # Cluster 3: Medium trips (3.9 days avg) with very high spending ($1,931/trip avg)
        elif 3 <= trip_duration <= 5 and receipts > 1500:
            return 2
        
        # Cluster 4: Medium trips (5.5 days avg) with high efficiency and low spending
        elif 4 <= trip_duration <= 7 and miles_per_day > 150 and receipts < 800:
            return 3
        
        # Cluster 5: Long trips (10.4 days avg) with high spending and medium efficiency
        elif trip_duration > 8 and receipts > 1200:
            return 4
        
        # Cluster 6: Short trips (4.4 days avg) with low efficiency and spending
        else:
            return 5
    
    def apply_rounding_bug(self, amount, receipt_ends_49_99):
        """
        FIXED: Apply the critical rounding bug with correct magnitude
        The bug should be subtle, not massive like before
        """
        if receipt_ends_49_99:
            # The bug: system adds a small bonus for receipts ending in .49 or .99
            # Based on R optimization: much smaller effect than before
            bug_bonus = self.config.bug_base_bonus + (amount * self.config.bug_multiplier)
            return amount + bug_bonus
        return amount
    
    def calculate_base_reimbursement(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Calculate base reimbursement using R-optimized parameters
        """
        # Base per diem using R-optimized parameter
        base_per_diem = self.config.base_per_diem * trip_duration_days
        
        # Mileage calculation with R-optimized rates
        if miles_traveled <= 100:
            mileage_reimbursement = miles_traveled * self.config.mileage_rates[0]
        elif miles_traveled <= 500:
            mileage_reimbursement = 100 * self.config.mileage_rates[0] + (miles_traveled - 100) * self.config.mileage_rates[1]
        else:
            mileage_reimbursement = 100 * self.config.mileage_rates[0] + 400 * self.config.mileage_rates[1] + (miles_traveled - 500) * self.config.mileage_rates[2]
        
        # Receipt reimbursement with R-optimized rates
        if total_receipts_amount <= 50:
            receipt_reimbursement = total_receipts_amount * self.config.receipt_rates[0]
        elif total_receipts_amount <= 500:
            receipt_reimbursement = 50 * self.config.receipt_rates[0] + (total_receipts_amount - 50) * self.config.receipt_rates[1]
        elif total_receipts_amount <= 1500:
            receipt_reimbursement = 50 * self.config.receipt_rates[0] + 450 * self.config.receipt_rates[1] + (total_receipts_amount - 500) * self.config.receipt_rates[2]
        else:
            receipt_reimbursement = 50 * self.config.receipt_rates[0] + 450 * self.config.receipt_rates[1] + 1000 * self.config.receipt_rates[2] + (total_receipts_amount - 1500) * self.config.receipt_rates[3]
        
        return base_per_diem + mileage_reimbursement + receipt_reimbursement
    
    def apply_cluster_adjustments(self, base_amount, cluster, features):
        """
        Apply cluster-specific adjustments based on R analysis
        """
        vacation_penalty = features[0][18]  # Actually a bonus per R analysis
        
        if cluster == 4 and vacation_penalty:  # Long trips, high spending with vacation bonus
            return base_amount * self.config.vacation_bonus_multiplier
        elif cluster == 4:  # Long trips, high spending without vacation bonus
            return base_amount * self.config.vacation_base_multiplier
        else:
            return base_amount * self.config.cluster_multipliers[cluster]
    
    def apply_threshold_effects(self, amount, features):
        """
        Apply threshold effects discovered in R analysis
        """
        trip_duration = features[0][0]
        miles_traveled = features[0][1]
        total_receipts = features[0][2]
        miles_per_day = features[0][5]
        is_5_day_trip = features[0][12]
        high_spending_flag = features[0][10]
        
        # Remove the high spending penalty for now - need better logic
        # The issue might be in the base calculation instead
        
        # 5-day sweet spot (strongest threshold effect from R analysis)
        if is_5_day_trip:
            amount *= self.config.five_day_multiplier
        
        # Receipt amount threshold at $660.54 (from R analysis)
        if total_receipts > 660.54:
            amount *= self.config.receipt_threshold_multiplier
        
        # Miles per day efficiency threshold at 187.01 (from R analysis)
        if miles_per_day > 187.01:
            amount *= self.config.efficiency_threshold_multiplier
        
        # Miles traveled threshold at 473.8 (from R analysis)
        if miles_traveled > 473.8:
            amount *= self.config.miles_threshold_multiplier
        
        return amount
    
    def calculate_reimbursement(self, trip_duration_days, miles_traveled, total_receipts_amount):
        """
        Main calculation method with enhanced logic from R analysis
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
        
        # Apply the critical rounding bug (FIXED)
        receipt_ends_49_99 = features[0][11]
        final_amount = self.apply_rounding_bug(threshold_adjusted, receipt_ends_49_99)
        
        # Apply interaction effects (top correlations from R analysis)
        duration_spending_interaction = features[0][23]
        log_receipts = features[0][32]
        
        # Duration × spending interaction (0.704 correlation from R analysis)
        interaction_bonus = duration_spending_interaction * self.config.interaction_coeff
        
        # Log receipts effect (0.718 correlation - highest from R analysis!)
        log_bonus = log_receipts * self.config.log_bonus_coeff
        
        final_amount += interaction_bonus + log_bonus
        
        # Ensure reasonable bounds and round to 2 decimal places
        final_amount = max(50.0, min(final_amount, 5000.0))
        return round(final_amount, 2)

def main():
    if len(sys.argv) != 4:
        print("Usage: python3 calculate_reimbursement_optimized.py <trip_duration_days> <miles_traveled> <total_receipts_amount>")
        sys.exit(1)
    
    try:
        trip_duration_days = int(sys.argv[1])
        miles_traveled = float(sys.argv[2])
        total_receipts_amount = float(sys.argv[3])
        
        # Validate inputs
        if trip_duration_days <= 0 or miles_traveled < 0 or total_receipts_amount < 0:
            raise ValueError("Invalid input values")
        
        # Create and use the optimized legacy system replica
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
