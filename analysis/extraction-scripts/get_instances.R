###############################################################################
# Implementation of an R script which is used to download instance data
#
# Author: Vivek Katial
# Created 2020-05-13 23:48:16
###############################################################################


# Setting Up --------------------------------------------------------------

library(tidyverse)
library(mlflow)
library(yaml)
source("utils/mlflow-utils.R")

# Global Vars
DATA_PATH = "data/d_runs.csv"

# Read experiment config file
exp_config <- read_yaml("config/mlflow-tracking-server.yml")
# Set MLFlow tracking URI
mlflow_set_tracking_uri(exp_config$mlflow$tracking_server_uri)

# Read in MLFlow data (AFTER having run `get_mlflow_data.py`)
d_runs <- get_mlflow_data(DATA_PATH)

#' This function downloads the clause data from S3
#' @param run_id The `run_id` for the experiment
get_clause_data = function(run_id){
  tmp_path <- mlflow_download_artifacts("d_clauses.rds", run_id = run_id)
  d_clause <- read_rds(tmp_path)
  file.remove(tmp_path)
  d_clause
}


# Enrich Instances --------------------------------------------------------

d_instances <- d_runs %>% 
  head() %>% 
  mutate(d_clauses = map(run_id, get_clause_data))

d_instances %>% 
  mutate(
    f_p_size_n_clauses = map_dbl(d_clauses, length),
    f_p_size_n_variables = n_qubits,
    f_p_size_ratio = metrics_clause_var_ratio,
    f_p_size_ratio_sq = metrics_clause_var_ratio^2,
    f_p_size_ratio_cub = metrics_clause_var_ratio^3,
    f_p_size_ratio_recp = 1/metrics_clause_var_ratio,
    f_p_size_ratio_recp_sq = (1/metrics_clause_var_ratio)^2,
    f_p_size_ratio_recp_cub = (1/metrics_clause_var_ratio)^3,
    f_p_size_lin_ratio = abs(4.26 - metrics_clause_var_ratio),
    f_p_size_lin_ratio_sq = abs(4.26 - metrics_clause_var_ratio)^2,
    f_p_size_lin_ratio_cb = abs(4.26 - metrics_clause_var_ratio)^3
)
