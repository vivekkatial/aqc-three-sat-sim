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
library(mlflow)

# Import helper scripts
source("utils/mlflow-utils.R")

# Global Variables for `mlflow`
EXPERIMENT="three-sat-usa"
MLFLOW_TRACKING_URI="http://115.146.95.176:5000/"


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
  
  #' TODO: Implement a test to ensure everything in the 
  #' script for  
  
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
    stop(sprintf("Parameters '%s' not in template file", setdiff(spec_params, template_params)))
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
    cat("WARNING: Directory 'params/ready/' not  found.")
    dir.create("params/ready/")
  }
  
  # Construct filename
  file_name <- file.path("params", "ready",  parameter_file_name)
  
  # Write file out to path
  write_file(parameter_content, file_name)
}







# FILTER GRID FOR IMPOSSIBLE VALUES ---------------------------------

#' The rules we have are the following:
#' 1. Number of Qubits > N_SAT (length of satisfiability clause)
#' 
#' I have written a function apply_experiment_rules() which  will
#' act on the parameter grid, this will  ensure that redundant experiments don't occur

#' This  function applies the rules outlined above
#' @param param_grid Parameter grid dataframe
#' @return The same grid with invalid row items removed
apply_experiment_rules = function(param_grid){
  
  if ( FALSE %in%  (c("parameter_filename", "parameter_file_content") %in% names(param_grid))) {
    stop("Ensure parameter grid is  valid")
  }
  
  # Apply rules specific to this experiment
  param_grid %>% 
    filter(n_qubits > n_sat)
}


# FILTER GRID FOR LARGE T and Small t -------------------------------------

#' In this scenario for large evolution time (T > 50) we wont use a small time step (0.01)
#' 
#' 1. For time step > 50: (T = 75, t = 0.1), (T = 100, t = 0.1)
#' 2. For time step < 50: (T = 10, t = 0.01), (T = 10, t = 0.1), ... , (T = 50, t = 0.1)
#' @param param_grid
#' @return A param grid with invalid values filtered out
apply_large_time_filter = function(param_grid){
  
  param_grid <- param_grid %>% 
    mutate(large_time_filter = ifelse(time_T > 50 & t_step == 0.01, "remove", "keep")) %>% 
    filter(large_time_filter == "keep") %>% 
    select(-large_time_filter)
  
  param_grid
  
}


# Removed Finished Runs ---------------------------------------------------

apply_remove_finished_runs <- function(param_grid){
  
  # Get Mlflow Data
  d_runs <- get_mlflow_data(EXPERIMENT, MLFLOW_TRACKING_URI)
  
  # Create vector of finished runs
  finished_runs <- d_runs %>% 
    select(file_name) %>% 
    transmute(file_name = str_replace(file_name, "params/ready/", "")) %>% 
    pull(file_name)
  
  param_grid %>% 
    filter(!(parameter_filename %in% finished_runs))
}





# Parse Vectors -----------------------------------------------------------



# Negative look-behind REGEX 
# Based on: https://stackoverflow.com/questions/7124778/how-to-match-anything-up-until-this-sequence-of-characters-in-a-regular-expres
.extract_vector_var = function(vector_def){
  vector_def %>% 
    str_extract(".+?(?=\\s&&\\s)")  
}

# Positive look-behind REGEX https://stackoverflow.com/questions/4419000/regex-match-everything-after-question-mark
# .extract_vector_val = function(vector_def){
#   vector_def %>% 
#     str_extract("(?<=\\s&&\\s).*")
# }

.extract_vector_val = function(vec_str){
  
  #browser()

  # Base index
  base_ind = vec_str %>% 
    str_extract(".+?(?=:)") %>% 
    as.numeric()
  
  # Final Index
  final_ind = vec_str %>% 
    str_extract("(?<=:).*") %>% 
    as.numeric()
  
  # Define vec
  vector = seq(base_ind, final_ind, by=1)
  list(vector)
}
  

.extract_vector_var = function(x, ls){
  
  map_chr(x, function(vec){
    ls[[vec]] %>% 
      names()
  })
}

#' @param param_spec Specification file read in as `read_yaml`
#' @return param_grid `tibble()` consisting of the parameters
build_param_grid = function(param_spec){
  
  # Build a nested dataframe (vectors nested as columns)
  d_params_nested <- param_spec %>% 
    cross_df() %>% 
    rowwise() %>% 
    mutate_at(vars(starts_with("VECTOR_")), .extract_vector_val) %>% 
    rename_at(vars(starts_with("VECTOR_")), list(~.extract_vector_var(x = ., ls=param_spec)))
  
  # Extract parameter types 
  d_types <- map(d_params_nested, class)
  
  # Identify column names
  col_names <- d_params_nested %>% names()
  
  # Initialise iterator
  i = 1
  
  # Loop over each col
  for (col_type in d_types) {
    # Identify list columns
    if (col_type == "list") {
      
      # Unnest relevant columns
      print(sprintf("Unnesting column %s", col_names[i]))
      
      # Unnest `list` columns
      d_params_nested <- d_params_nested %>% 
        unnest(col_names[i])
    }
    
    # Iterate
    i = i + 1
  }
  
  d_params_nested
}


d_params_grid <- build_param_grid(param_spec)


# Build Grid --------------------------------------------------------------

param_grid_final <- d_params_grid %>% 
  mutate(
    parameter_filename         = pmap_chr(., make_parameter_filename) ,
    parameter_file_content     = pmap_chr(., make_parameter_file)
  ) %>% 
  # Apply rules to parameter search
  apply_experiment_rules() %>% 
  apply_large_time_filter() %>% 
  apply_remove_finished_runs() %>% 
  # Arrange according to experiment size
  arrange(n_qubits, desc(t_step), time_T)


# Create TEST FILE
test_file <- param_grid_final %>% 
  arrange_all() %>% 
  slice(1)

mapply(
  .write_parameter_file,
  "EXPERIMENT_TEST.YML",
  test_file$parameter_file_content
  )

#  Write out  to  files
mapply(
  .write_parameter_file, 
  param_grid_final$parameter_filename, 
  param_grid_final$parameter_file_content
  )

