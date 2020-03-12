#!bin/bash

#####################################
# This script is to install	    #
# Singularity on a Nectar Cloud VM  #
#				    #
# Author: Vivek Katial		    #
#####################################

# Set tags
set -e
set -v

# Download system dependencie
sudo apt-get update && sudo apt-get install -y \
    build-essential \
    libssl-dev \
    uuid-dev \
    libgpgme11-dev \
    squashfs-tools \
    libseccomp-dev \
    wget \
    pkg-config \
    cryptsetup \
    git

# Download Go
sudo snap install go --classic

# Set environment variable
echo 'export PATH=/usr/local/go/bin:$PATH' >> ~/.bashrc && \
  source ~/.bashrc

# Download latest stable release from GitHub
export VERSION=3.4.0 # adjust this as necessary

# Move to root dir
cd ~/

# Download singularity
wget https://github.com/sylabs/singularity/releases/download/v${VERSION}/singularity-${VERSION}.tar.gz
tar -xzf singularity-${VERSION}.tar.gz && \
cd singularity

# Compile the Singularity Source Code
./mconfig && \
    make -C builddir && \
    sudo make -C builddir install

