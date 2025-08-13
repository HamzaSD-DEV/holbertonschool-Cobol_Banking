#!/bin/bash
#########################################################################
# Dataset Reset Script - Reset all datasets to original state
# Useful for instructors to reset environment between student sessions
#########################################################################

echo "🔄 JCL Framework - Dataset Reset"
echo "================================"
echo "Resetting all datasets to original state..."
echo ""

# Clear all existing datasets
echo "🗑️  Clearing existing datasets..."
rm -rf datasets/*.dat
rm -rf datasets/gdg/*
rm -rf datasets/vsam/*

# Recreate directory structure
mkdir -p datasets/gdg/demo_reports
mkdir -p datasets/vsam
mkdir -p datasets/temp

# Run setup to recreate all datasets
echo "🚀 Recreating datasets..."
./setup_datasets.sh

echo ""
echo "✅ Dataset reset complete!"
echo "📚 All datasets restored to original state"
echo "🎯 Ready for fresh student session"