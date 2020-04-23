###############################################################################
# Investigating Instance chars
#
# Author: Vivek Katial
# Created 2020-03-31 12:29:50
###############################################################################

library(mlflow)
library(tidyverse)

# Global Vars
MLFLOW_TRACKING_URI="http://115.146.95.176:5000/"
EXPERIMENT="three-sat-usa"

mlflow::mlflow_set_tracking_uri(MLFLOW_TRACKING_URI)
d_runs <- mlflow_list_run_infos(experiment_id = 2)

clean_metric = function(run_id, metric){
  resp = mlflow_get_metric_history(metric_key = metric, run_id = run_id)
  resp$value
}

d_runs %>% 
  filter(status == "FINISHED") %>% 
  mutate(run_info = map(run_id, mlflow_get_run)) %>% 
  clean_runs()


