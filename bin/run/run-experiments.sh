#!/bin/bash

# --------------------------------------------
# This script spins up diff experiments
#
# Author: Vivek Katial
# --------------------------------------------

set -e

# Set env vars
source bin/init/set-environment-variables.sh

# Set the directory in which experiments parameter files stored
export EXPERIMENT_FILE_DIR=$S3_BUCKET/$EXPERIMENT_NAME/ready/
echo "$EXPERIMENT_FILE_DIR"

for file in $(aws s3 ls $EXPERIMENT_FILE_DIR --endpoint-url=$MLFLOW_S3_ENDPOINT_URL | awk '{print $4}'); do
     # Copy file
     echo "Copying experiment $EXPERIMENT_FILE_DIR$file params/ready/."
     
     # Pull file into local directory
     aws s3 cp $EXPERIMENT_FILE_DIR$file params/ready/ --endpoint-url=$MLFLOW_S3_ENDPOINT_URL

     # Experiment name
     experiment_name=${file::${#file}-4}
     
     # Define run_file
     export run_file=params/ready/$file
     echo -e "Submitting job: \t $run_file"
     # Run experiment as an instance of the singularity container
     sbatch --output=$file.log bin/run_experiment.slurm $run_file

     exit

done


