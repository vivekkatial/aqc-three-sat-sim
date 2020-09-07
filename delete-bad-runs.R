###############################################################################
# Script to delete BAD runs
#
# Author: Vivek Katial
# Created 2020-09-01 17:17:42
###############################################################################

library(tidyverse)
library(mlflow)
source("utils/mlflow-utils.R")
source("tests/testthat/test-hamiltonians.R")

check_clause = function(clause_path, n_qubits){
  clause_path = file.path("data", "clause-instances", clause_path)
  # Read in clause
  d_clauses <- read_rds(clause_path)
  # Create Hamiltonian
  
  result = tryCatch({
    ham <- create_ham_problem(n_qubits, d_clauses)
    test_problem_ham(ham, experiment_name = "usa-relaxed")
  }, error = function(e) {
    return("BUG")
  })
}


d_runs <- read_csv("data/d_runs.csv")

d_bugged_runs <- list.files("data/clause-instances/") %>% 
  as_tibble() %>% 
  rename(clause_file = value) %>% 
  mutate(run_id = str_remove_all(clause_file, "d_clauses_|.rds")) %>% 
  left_join(d_runs, by = "run_id") %>% 
  filter(!is.na(params.n_qubits)) %>% 
  mutate(BUG = map2_chr(clause_file, params.n_qubits, check_clause))

d_bugged_runs %>% 
  count(BUG)


# Delete all runs which are Bugged on MLFLOW
bad_runs <- d_bugged_runs %>% 
  filter(BUG == "BUG") %>% 
  pull(run_id)
  
# Delete on MLFlow
map(bad_runs, mlflow_delete_run)

d_runs %>% 
  filter(!(run_id %in% bad_runs)) %>% 
  write_csv("data/d_runs.csv")
