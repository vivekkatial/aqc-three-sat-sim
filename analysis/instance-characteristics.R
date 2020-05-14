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
DATA_PATH = "data/d_runs.csv"

# Get Mlflow Data
d_runs <- get_mlflow_data(DATA_PATH)


# Experiments Run ---------------------------------------------------------

d_runs %>% 
  count(n_qubits, t_step, time_t) %>% 
  arrange(n) %>% 
  ggplot(aes(x = as.factor(n_qubits), y = n, fill = as.factor(time_t))) + 
  geom_col(aes(fill = as.factor(time_t)), position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.7, size = 2.6) + 
  scale_fill_brewer(palette = "Blues", name = "Evolution Time") +
  theme_light() +
  facet_wrap(~t_step) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "n qubits",
    y = "Number of runs",
    title = "Summary of Experiments Run "
  )


# Energy and Entanglement vs n_qubits -------------------------------------

d_runs %>% 
  group_by(n_qubits) %>% 
  summarise(
    entanglement = mean(metrics_max_shannon_entropy,na.rm = T),
    min_energy = mean(metrics_min_energy_gap, na.rm = T)
  ) %>% 
  gather(metric, value, -n_qubits) %>% 
  ggplot(aes(x = n_qubits, y = value, group = 1)) + 
  geom_line() + 
  facet_wrap(~metric, scales = "free", ncol = 1) + 
  theme_light() +
  labs(
    x = "n_qubits"
  )


d_runs %>% 
  select(metrics_clause_var_ratio, metrics_min_energy_gap, metrics_max_shannon_entropy, n_qubits, time_t) %>% 
  mutate_all(as.numeric) %>% 
  mutate(
    n_qubits = as.factor(n_qubits),
    time_t = as.factor(time_t)
    ) %>% 
  ggplot(aes(x = metrics_clause_var_ratio, y = metrics_max_shannon_entropy, col = time_t)) +
  geom_point(alpha = 0.8) + 
  facet_wrap(~n_qubits) + 
  #scale_color_brewer(palette = "Blues") + 
  theme_light() + 
  labs(
    x = "Clause-Variable Ratio",
    y = "Max Shannon Entropy",
    title = "Shannon Entropy vs Clause-Var Ratio (by qubits)"
  )

  

# P(success) over T -------------------------------------------------------

d_runs %>% 
  select(metrics_p_success, n_qubits, time_t) %>% 
  mutate(time_t = as.numeric(time_t)) %>% 
  ggplot(aes(x = time_t, y = metrics_p_success)) + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~as.numeric(n_qubits)) +
  labs(
    y = "Probability of Success",
    x = "Evolution Time"
  ) + 
  theme_light()


# P(success) vs Entropy ---------------------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_max_shannon_entropy, time_t) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_max_shannon_entropy, col = as.factor(time_t))) + 
  geom_point(alpha = 0.8) +
  labs(
    x = "Probability of Success",
    y = "Shannon Entropy"
  ) +
  theme_light()


# P(success) vs Entropy (by Qubits) ---------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_max_shannon_entropy, time_t, n_qubits) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_max_shannon_entropy, col = as.factor(time_t))) + 
  geom_point(alpha = 0.6) +
  facet_wrap(~n_qubits) + 
  labs(
    x = "Probability of Success",
    y = "Shannon Entropy",
    title = " P(success) vs Entropy (by Qubits)"
  ) +
  theme_light()


# P(success) vs Min Energy Gap --------------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_min_energy_gap, time_t) %>% 
  mutate(time_t = as.factor(time_t)) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_min_energy_gap, col = time_t)) + 
  geom_point(alpha = 0.8) +
  theme_light() +
  labs(
    x = "Probability of Success",
    y = "Min Energy Gap",
    title =  "P(success) vs Min Energy Gap"
  )


# P(Success) vs Energy Gap (by Qubit) -------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_min_energy_gap, time_t, n_qubits) %>% 
  mutate(time_t = as.factor(time_t)) %>% 
  ggplot(aes(x = metrics_p_success, y = metrics_min_energy_gap, col = time_t)) + 
  geom_point(alpha = 0.8) +
  facet_wrap(~n_qubits) + 
  theme_light() + 
  labs(
    x = "Probability of Success",
    y = "Min Energy Gap",
    title = "P(Success) vs Energy Gap (by Qubit)"
  )


# P(success) vs Clause to Var Ratio ---------------------------------------

d_runs %>% 
  select(metrics_p_success, metrics_clause_var_ratio, n_qubits, time_t) %>% 
  mutate(time_t = as.factor(time_t)) %>% 
  ggplot(aes(x = metrics_clause_var_ratio, y = metrics_p_success, col = time_t)) + 
  geom_point() + 
  facet_wrap(~n_qubits) +
  theme_light() + 
  labs(
    x = "Clause-Variable Ratio",
    y = "P(Success)"
  )

