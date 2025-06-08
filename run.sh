#!/bin/bash

# Black Box Challenge - Legacy Reimbursement System Replica
# This script replicates a 60-year-old travel reimbursement system
# Usage: ./run.sh <trip_duration_days> <miles_traveled> <total_receipts_amount>

# Implementation: Optimized R-based reverse-engineered legacy system
# Uses DEoptim-optimized parameters for maximum accuracy
Rscript calculate_reimbursement.R "$1" "$2" "$3"
