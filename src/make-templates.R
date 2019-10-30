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
  
  # Check that all parameters in param_spec are also present in param template!
  spec_params <- params %>% 
    names()
  
  template_params <- param_template %>% 
    str_extract_all(., "\\{\\{.*\\}\\}") %>% 
    unlist() %>% unique() %>% 
    str_remove_all("\\{|\\}")
  
  if (spec_params %in% template_params %>% sum() != length(spec_params)) {
    stop(printf("Parameters '%s' not in template file", setdiff(spec_params, template_params)))
  }
  
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
  
  # Check if all necessary folders are present!
  if (!dir.exists("params/ready")) {
    stop("Directory 'params/ready/' not  found.")
  }
  
  # Construct filename
  file_name <- file.path("params", "ready",  parameter_file_name)
  
  # Write file out to path
  write_file(parameter_content, file_name)
}







# FILTER GRID FOR IMPOSSIBLE VALUES ---------------------------------

#' The rules we have are the following:
#' 1. Number of Qubits > k (number of clauses)
#' 2. Number of Qubits > N_SAT (length of satisfiability clause)
#' 
#' I have written a function apply_experiment_rules() which  will
#' act on the parameter grid, this will  ensure that redundant experiments don't occur

#' This  function applies the rules outlined above
#' @param param_grid Parameter grid dataframe
#' @return The same grid with invalid row items removed
apply_experiment_rules <- function(param_grid){
  
  if ( FALSE %in%  (c("parameter_filename", "parameter_file_content") %in% names(param_grid))) {
    stop("Ensure parameter grid is  valid")
  }
  
  # Apply rules specific to this experiment
  param_grid %>% 
    filter(
      n_qubits > n_sat,
      n_qubits > k
      )
}


# Build Grid --------------------------------------------------------------

param_grid <- param_spec %>% 
  cross_df() %>% 
  mutate(
    parameter_filename         = pmap_chr(., make_parameter_filename),
    parameter_file_content     = pmap_chr(., make_parameter_file)
  ) %>% 
  apply_experiment_rules()

#  Write out  to  files
mapply(
  .write_parameter_file, 
  param_grid$parameter_filename, 
  param_grid$parameter_file_content
  )

