###############################################################################
# Generates 3 SAT Template Files for experimentation
#
# Author: Vivek Katial
# Created 2019-10-11 19:17:10
###############################################################################



# Import Dependencies -----------------------------------------------------

# Import libraries
library(yaml)
library(tidyverse)

# Read in relavent files
param_spec <- read_yaml("params/params_spec.yml")
param_template <- read_yaml("params/params_template.yml")

# Function to generate parameter_file -------------------------------------------

make_parameter_file <- function(...){
  browser()
}




# Build Grid --------------------------------------------------------------

param_grid <- param_spec %>% 
  cross_df() %>% 
  mutate(
    parameter_file = pmap(., make_parameter_file)
  )

param_grid
