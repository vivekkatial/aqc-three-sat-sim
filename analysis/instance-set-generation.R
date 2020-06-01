###############################################################################
# Script to generate instance set for analysis
#
# Author: Vivek Katial
# Created 2020-06-02 00:38:55
###############################################################################

# Source in scripts
library(here)
library(tidyverse)

# Source in relavent scripts
source(here("src/instance-generation/generate-relaxed-usa-instances.R"))
source(here("src/instance-generation/generate-usa-instances.R"))


d_instance_set <- list(
  n_qubits = 5:15,
  n_sat = 3,
  instance_index = 1:30
) %>% 
  cross_df() %>% 
  mutate(
    params = map2(n_qubits, n_sat, function(x, y)list(n_qubits = x, n_sat = y)),
    d_clauses_usa = map(params, generate_clauses),
    d_clauses_usa_relaxed = map(params, generate_relaxed_usa_clauses)
  )

d_instance_set %>% 
  write_rds("data/d_instance_set.rds")
