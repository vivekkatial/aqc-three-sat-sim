###############################################################################
# Mlflow Utility Functions
#
# Author: Vivek Katial
# Created 2020-04-24 09:00:10
###############################################################################

#' Function to get data from MLFlow
#' @param experiment_name `character` Name of the experiment you're wanting
#' @param tracking_uri `character` URI for mlflow tracking server
#' @return A dataframe consisting of run_info for each `FINISHED` run
get_mlflow_data = function(experiment_name, tracking_uri, ...){
  
  mlflow::mlflow_set_tracking_uri(tracking_uri)
  
  # Find Name of Experimement ID
  experiment_id = mlflow_list_experiments() %>% 
    filter(name == experiment_name) %>% 
    pull(experiment_id)
  
  assertthat::are_equal(length(experiment_id), 1)
  
  d_cleaned_runs <- mlflow_list_run_infos(experiment_id = experiment_id) %>% 
    filter(status == "FINISHED") %>% 
    mutate(run_info = map(run_id, mlflow_get_run)) %>% 
    .clean_runs()
  
}



#' Utility function to clean the runs dataframe from mlflow
#' @param d_runs A dataframe consisting of run_info collected from mlflow API
#' @return cleaned dataframe
.clean_runs = function(d_runs){
  
  # Clean df
  d_cleaned_runs <- d_runs %>% 
    select(run_info) %>% 
    unnest(cols = c(run_info)) %>% 
    unnest(metrics) %>% 
    # Widen cols for extracting metrics
    pivot_wider(
      id_cols = c(run_uuid, start_time, end_time, artifact_uri, params),
      names_from = key, 
      values_from = value,
      names_prefix = "metrics_"
    ) %>% 
    unnest(params) %>% 
    spread(key, value)
  
  # Return key metircs
  d_cleaned_runs
}
