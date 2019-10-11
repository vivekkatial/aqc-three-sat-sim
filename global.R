###############################################################################
# Entry point to shiny application
#
# Created Date: Mon Sep  2 12:55:53 2019
# Author: Vivek Katial
############################################################################### 

import_library = function(lib_name){
  suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))
}

import_library("logging")
import_library("tidyverse")
import_library("shiny")
import_library("yaml")
import_library("complexplus")
import_library("plotly")
import_library("shinycssloaders")
import_library("shinymaterial")
import_library("argonDash")
import_library("argonR")

# Source Utils
source("utils/exp-utils.R")
source("utils/define-pauli-matrices.R")
source("utils/schrodinger-solver.R")


exp_param_file <- "params/params_template.yml"

# Begin our 3SAT Experiment
loginfo("Starting Application with conifguration: '%s'", exp_param_file)

# Sourcing params
params <- extract_params(exp_param_file)

# Setting scripts
source_exp_scripts(params)
