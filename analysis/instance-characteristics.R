###############################################################################
# Investigating Instance chars
#
# Author: Vivek Katial
# Created 2020-03-31 12:29:50
###############################################################################

library(mlflow)
library(tidyverse)
source("utils/mlflow-utils.R")

# Global Vars
EXPERIMENT="three-sat-usa"
MLFLOW_TRACKING_URI="http://115.146.95.176:5000/"

# Get Mlflow Data
d_runs <- get_mlflow_data(EXPERIMENT, MLFLOW_TRACKING_URI)

d_runs %>% 
  filter(t_step == 0.01, time_T == 100) %>% 
  group_by(n_qubits) %>% 
  summarise(
    entanglement = mean(metrics_max_shannon_entropy,na.rm = T),
    min_energy = mean(metrics_min_energy_gap, na.rm = T)
  ) %>% 
  gather(metric, value, -n_qubits) %>% 
  ggplot(aes(x = n_qubits, y = value, group = 1)) + 
  geom_line() + 
  facet_wrap(~metric, scales = "free", ncol = 1)

d_runs %>% 
  select(metrics_clause_var_ratio, metrics_min_energy_gap, metrics_max_shannon_entropy, n_qubits) %>% 
  mutate_all(as.numeric) %>% 
  ggplot(aes(x = n_qubits, y = metrics_clause_var_ratio)) +
  geom_point(aes(col = metrics_max_shannon_entropy))

  
