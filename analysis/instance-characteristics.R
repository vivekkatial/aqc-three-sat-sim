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
EXPERIMENT="three-sat"

mlflow::mlflow_set_tracking_uri(MLFLOW_TRACKING_URI)

mlflow_list_experiments()
d_runs <- mlflow_list_run_infos(run_view_type = "ACTIVE", experiment_id = 1)

d_runs %>% 
  filter(!is.na(end_time)) %>% 
  mutate(metrics = mlflow::mlflow_get_metric_history)
