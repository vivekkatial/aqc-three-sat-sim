###############################################################################
# Mlflow Utility Functions
#
# Author: Vivek Katial
# Created 2020-04-24 09:00:10
###############################################################################

#' Function to get data from MLFlow
#' @param data_path `character` Path containing data file
#' @return A dataframe consisting of run_info for each `FINISHED` run
get_mlflow_data = function(data_path, ...){
  
  # Log information
  logging::loginfo("Data last updated %s", file.info("data/d_runs.csv")$ctime)
  
  d_runs <- read_csv("data/d_runs.csv") %>% 
    filter(status == "FINISHED") %>% 
    janitor::clean_names() %>% 
    rename_at(vars(contains("params_")), function(x) str_remove_all(x, "params_"))
  
}

#' This function downloads the clause data from S3
#' @param run_id The `run_id` for the experiment
get_clause_data = function(run_id){
  # Name instance file
  instance_file  = sprintf("data/clause-instances/d_clauses_%s.rds", run_id)
  # Check if it exists locally
  if (file.exists(instance_file)) {
    d_clause = read_rds(instance_file)
    d_clause
  } else {
    # Download from MLFlow
    tmp_path <- mlflow_download_artifacts("d_clauses.rds", run_id = run_id)
    d_clause <- read_rds(tmp_path)
    d_clause %>% 
      write_rds(path = instance_file)
    file.remove(tmp_path)
    d_clause
  }
}
