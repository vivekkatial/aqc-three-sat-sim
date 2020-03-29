#!/bin/bash

#SBATCH --job-name=run-experiment
#SBATCH -p cloud

# For R need to run on single CPU
#SBATCH --ntasks=1

# set your minimum acceptable walltime=hours:minutes:seconds
#SBATCH -t 0:15:00

# How much memory you need.
# --mem will define memory per node and
# --mem-per-cpu will define memory per CPU/core. Choose one of those.
#SBATCH --mem=31000MB

# Specify your email address to be notified of progress.
### SBATCH --mail-user=vkatial@student.unimelb.edu.au
### SBATCH --mail-type=ALL

# Load in Singularity Model
module load Singularity/3.2.0-spartan_gcc-6.2.0

file_input=$1
echo "Running Experiment on log file $file_input"


# The command to actually run the job
singularity run --app run_experiment portable-image.img $file_input