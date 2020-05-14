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
