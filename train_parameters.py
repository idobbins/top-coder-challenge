#!/usr/bin/env python3
"""
Parameter Training Script for Legacy Reimbursement System
Optimizes parameters using grid search on public cases
"""

import json
import sys
from itertools import product
from statistics import mean
from calculate_reimbursement import LegacyReimbursementSystem, ModelConfig

def load_training_data(filename="public_cases.json"):
    """Load training data from JSON file"""
    with open(filename, 'r') as f:
        data = json.load(f)
    
    inputs = []
    targets = []
    for case in data:
        inputs.append([
            case['input']['trip_duration_days'],
            case['input']['miles_traveled'],
            case['input']['total_receipts_amount']
        ])
        targets.append(case['expected_output'])
    
    return inputs, targets

def evaluate_config(config, inputs, targets):
    """Evaluate a configuration on the training data"""
    system = LegacyReimbursementSystem()
    system.config = config
    
    errors = []
    for i, (inp, target) in enumerate(zip(inputs, targets)):
        try:
            prediction = system.calculate_reimbursement(inp[0], inp[1], inp[2])
            error = abs(prediction - target)
            errors.append(error)
        except Exception as e:
            # If calculation fails, assign a large error
            errors.append(1000.0)
    
    return mean(errors)

def analyze_high_error_cases(inputs, targets, current_config):
    """Analyze cases with highest errors to guide optimization"""
    system = LegacyReimbursementSystem()
    system.config = current_config
    
    case_errors = []
    for i, (inp, target) in enumerate(zip(inputs, targets)):
        try:
            prediction = system.calculate_reimbursement(inp[0], inp[1], inp[2])
            error = abs(prediction - target)
            case_errors.append((i, error, inp, target, prediction))
        except:
            case_errors.append((i, 1000.0, inp, target, 0))
    
    # Sort by error and return top 10
    case_errors.sort(key=lambda x: x[1], reverse=True)
    return case_errors[:10]

def optimize_parameters():
    """Main optimization function"""
    print("Loading training data...")
    inputs, targets = load_training_data()
    print(f"Loaded {len(inputs)} training cases")
    
    # Analyze current performance
    current_config = ModelConfig()
    current_error = evaluate_config(current_config, inputs, targets)
    print(f"Current average error: ${current_error:.2f}")
    
    # Analyze high-error cases
    print("\nAnalyzing high-error cases...")
    high_error_cases = analyze_high_error_cases(inputs, targets, current_config)
    
    print("Top 10 highest error cases:")
    for i, (case_idx, error, inp, target, prediction) in enumerate(high_error_cases):
        duration, miles, receipts = inp
        print(f"  {i+1}. Case {case_idx}: {duration} days, {miles} miles, ${receipts:.2f}")
        print(f"     Expected: ${target:.2f}, Got: ${prediction:.2f}, Error: ${error:.2f}")
        
        # Analyze trip characteristics
        miles_per_day = miles / duration
        receipts_per_day = receipts / duration
        print(f"     Miles/day: {miles_per_day:.1f}, Receipts/day: ${receipts_per_day:.2f}")
        print()
    
    # Define parameter search space based on error analysis
    print("Starting parameter optimization...")
    
    # Focus on parameters that affect the high-error cases
    param_ranges = {
        'cluster_3_multiplier': [1.0, 1.1, 1.2, 1.3, 1.4, 1.5],  # Medium efficiency, low spending
        'efficiency_threshold_multiplier': [1.02, 1.03, 1.05, 1.07, 1.10],  # High efficiency bonus
        'log_bonus_coeff': [1.5, 2.0, 2.5, 3.0, 3.5],  # Log receipts effect
        'base_per_diem': [45, 50, 55, 60],  # Base daily rate
        'mileage_rate_high': [0.25, 0.30, 0.35, 0.40]  # High mileage rate
    }
    
    best_error = float('inf')
    best_config = None
    best_params = None
    
    total_combinations = 1
    for param_range in param_ranges.values():
        total_combinations *= len(param_range)
    
    print(f"Testing {total_combinations} parameter combinations...")
    
    tested = 0
    for params in product(*param_ranges.values()):
        tested += 1
        if tested % 50 == 0:
            print(f"  Progress: {tested}/{total_combinations} ({100*tested/total_combinations:.1f}%)")
        
        # Create test configuration
        config = ModelConfig()
        config.cluster_multipliers[3] = params[0]  # Cluster 3 (medium efficiency, low spending)
        config.efficiency_threshold_multiplier = params[1]
        config.log_bonus_coeff = params[2]
        config.base_per_diem = params[3]
        config.mileage_rates[2] = params[4]  # High mileage rate (>500 miles)
        
        # Evaluate this configuration
        error = evaluate_config(config, inputs, targets)
        
        if error < best_error:
            best_error = error
            best_config = config
            best_params = params
            print(f"  New best: ${error:.2f} with params {params}")
    
    print(f"\nOptimization complete!")
    print(f"Best average error: ${best_error:.2f} (improvement: ${current_error - best_error:.2f})")
    print(f"Best parameters:")
    print(f"  cluster_3_multiplier: {best_params[0]}")
    print(f"  efficiency_threshold_multiplier: {best_params[1]}")
    print(f"  log_bonus_coeff: {best_params[2]}")
    print(f"  base_per_diem: {best_params[3]}")
    print(f"  mileage_rate_high: {best_params[4]}")
    
    # Test on high-error cases again
    print("\nTesting optimized parameters on previously high-error cases:")
    optimized_errors = analyze_high_error_cases(inputs, targets, best_config)
    
    for i, (case_idx, error, inp, target, prediction) in enumerate(optimized_errors[:5]):
        duration, miles, receipts = inp
        original_error = high_error_cases[i][1] if i < len(high_error_cases) else 0
        improvement = original_error - error
        print(f"  Case {case_idx}: Error ${error:.2f} (was ${original_error:.2f}, improvement: ${improvement:.2f})")
    
    return best_config

def generate_optimized_script(config):
    """Generate an optimized version of calculate_reimbursement.py with embedded parameters"""
    print("\nGenerating optimized script...")
    
    # Read the current script
    with open('calculate_reimbursement.py', 'r') as f:
        script_content = f.read()
    
    # Replace the ModelConfig __init__ method with optimized parameters
    new_init = f'''    def __init__(self):
        # Base calculation parameters (OPTIMIZED)
        self.base_per_diem = {config.base_per_diem}
        self.mileage_rates = {config.mileage_rates}
        self.receipt_rates = {config.receipt_rates}
        
        # Cluster adjustment multipliers (OPTIMIZED)
        self.cluster_multipliers = {config.cluster_multipliers}
        self.vacation_bonus_multiplier = {config.vacation_bonus_multiplier}
        self.vacation_base_multiplier = {config.vacation_base_multiplier}
        
        # Threshold effect multipliers (OPTIMIZED)
        self.five_day_multiplier = {config.five_day_multiplier}
        self.receipt_threshold_multiplier = {config.receipt_threshold_multiplier}
        self.efficiency_threshold_multiplier = {config.efficiency_threshold_multiplier}
        self.miles_threshold_multiplier = {config.miles_threshold_multiplier}
        
        # Interaction coefficients (OPTIMIZED)
        self.interaction_coeff = {config.interaction_coeff}
        self.log_bonus_coeff = {config.log_bonus_coeff}
        
        # Rounding bug parameters (OPTIMIZED)
        self.bug_base_bonus = {config.bug_base_bonus}
        self.bug_multiplier = {config.bug_multiplier}'''
    
    # Find and replace the __init__ method
    import re
    pattern = r'(class ModelConfig:.*?def __init__\(self\):)(.*?)(?=\n\nclass|\nclass)'
    
    def replace_init(match):
        return match.group(1) + new_init + '\n'
    
    optimized_content = re.sub(pattern, replace_init, script_content, flags=re.DOTALL)
    
    # Write optimized script
    with open('calculate_reimbursement_optimized.py', 'w') as f:
        f.write(optimized_content)
    
    print("Optimized script saved as 'calculate_reimbursement_optimized.py'")

if __name__ == "__main__":
    try:
        best_config = optimize_parameters()
        generate_optimized_script(best_config)
        
        print("\nTraining complete! To use the optimized version:")
        print("1. Replace calculate_reimbursement.py with calculate_reimbursement_optimized.py")
        print("2. Run ./eval.sh to test the improved performance")
        
    except FileNotFoundError:
        print("Error: public_cases.json not found. Make sure you're in the correct directory.")
        sys.exit(1)
    except Exception as e:
        print(f"Error during training: {e}")
        sys.exit(1)
