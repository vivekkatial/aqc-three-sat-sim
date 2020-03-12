#!bin/bash

# --------------------------------------------
# This script builds a Singularity container
#
# Author: Vivek Katial
# --------------------------------------------

# Build container
sudo singularity build portable-image.img SingularityFile.def
