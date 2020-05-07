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

  

# P(success) over T -------------------------------------------------------

d_runs %>% 
  select(metrics_p_success, n_qubits, time_T) %>% 
  mutate(time_T = as.numeric(time_T)) %>% 
  ggplot(aes(x = time_T, y = metrics_p_success)) + 
  geom_point(alpha = 0.4) + 
  facet_wrap(~as.numeric(n_qubits)) +
  labs(
    y = "Probability of Success"
  )


# P(success) vs Entropy ---------------------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_max_shannon_entropy) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_max_shannon_entropy, col = "USA")) + 
  geom_point() +
  labs(
    y = "Probability of Success"
  )


# P(success) vs Min Energy Gap --------------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_min_energy_gap, time_T) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_min_energy_gap, col = time_T)) + 
  geom_point() +
  labs(
    x = "Probability of Success"
  )

# P(success) vs Clause to Var Ratio ---------------------------------------

d_runs %>% 
  select(metrics_max_shannon_entropy, metrics_clause_var_ratio, n_qubits) %>% 
  ggplot(aes(x = metrics_clause_var_ratio, y = metrics_max_shannon_entropy, col = "USA")) + 
  geom_point() + 
  facet_wrap(~n_qubits)

