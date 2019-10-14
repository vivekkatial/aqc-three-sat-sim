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
param_template <- read_file("params/params_template.yml")

# Functions to generate parameter_file -------------------------------------------

make_parameter_filename = function(...){
  
  # Unpack parameters 
  params <- list(...)
  
  # Make file name
  names <- names(params)
  vals  <- params %>% 
    flatten_chr()
  
  # Construct file_name
  file_name = paste0(names, vals, collapse = "__") %>% 
    paste0(".yml")
  
  file_name
  
}


make_parameter_file = function(...){
  
  # Unpack parameters 
  params <- list(...)
  parameter_file = param_template
  
  # Read template file
  for (param in names(params)) {
    parameter_file = str_replace_all(
      parameter_file, 
      pattern = paste0("\\{\\{", param, "\\}\\}"), 
      replace = as.character(params[[param]])
    )
  }
  
  parameter_file
  
}

#' This function writes parameter files
#' We need to make sure a directory to hold  them is  there
#' @param parameter_file_name Name of  the parameter file (from parameter grid)
#' @param 
.write_parameter_file = function(parameter_file_name, parameter_content){
  
  # Construct filename
  file_name <- file.path("params", "ready",  parameter_file_name)
  
  # Write file out to path
  write_file(parameter_content, file_name)
}




# Build Grid --------------------------------------------------------------

param_grid <- param_spec %>% 
  cross_df() %>% 
  mutate(
    parameter_filename         = pmap_chr(., make_parameter_filename),
    parameter_file_content     = pmap_chr(., make_parameter_file)
  )

#  Write out  to  files
mapply(
  .write_parameter_file, 
  param_grid$parameter_filename, 
  param_grid$parameter_file_content
  )

