#!/bin/bash

# --------------------------------------------
# This test  script runs  the experiments
#
# Author: Vivek Katial
# --------------------------------------------

export EXPERIMENT_FILE_DIR=params/ready/

for file in "$EXPERIMENT_FILE_DIR"*; do
  echo "Running Experiment on $file"
  # Run Script
  Rscript main.R $file
done