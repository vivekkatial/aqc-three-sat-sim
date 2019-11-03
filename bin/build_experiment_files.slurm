#!/bin/bash

#SBATCH --job-name=build-experiment-configs
#SBATCH -p cloud

# For R need to run on single CPU
#SBATCH --ntasks=1

# set your minimum acceptable walltime=hours:minutes:seconds
#SBATCH -t 0:15:00

# Specify your email address to be notified of progress.
### SBATCH --mail-user=vkatial@student.unimelb.edu.au
### SBATCH --mail-type=ALL

# Load in Singularity Model
module load Singularity/3.2.0-spartan_gcc-6.2.0

# The command to actually run the job
singularity exec portable-image.img 

