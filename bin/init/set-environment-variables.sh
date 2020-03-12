#!/bin/bash

# --------------------------------------------
# This script sets all environment variables
# required for one run of this experiment
#
# Author: Vivek Katial
# --------------------------------------------

echo "Setting Environment Variables for Experiment"

# --------------------------------------------
# Experiment ENV Variables
# --------------------------------------------
EXPERIMENT_NAME=aqc-three-sat-sim

# --------------------------------------------
# MLFLOW Environment Variables
# --------------------------------------------
MLFLOW_S3_ENDPOINT_URL=https://objects.storage.unimelb.edu.au
MLFLOW_PYTHON_BIN=/usr/local/bin/python
MLFLOW_BIN=/usr/local/bin/mlflow
S3_BUCKET=s3://testBucket

# Export env vars
export EXPERIMENT_NAME \
    MLFLOW_S3_ENDPOINT_URL \
    MLFLOW_PYTHON_BIN \
    MLFLOW_BIN \
    S3_BUCKET

printenv | grep EXPERIMENT_NAME
printenv | grep MLFLOW_S3_ENDPOINT_URL
printenv | grep MLFLOW_PYTHON_BIN
printenv | grep MLFLOW_BIN
printenv | grep S3_BUCKET
