###############################################################################
# Main entrypoint script for running an experiment with 3SAT
#
# Created Date: Wed Aug 28 16:09:48 2019
# Author: Vivek Katial
############################################################################### 

import_library = function(lib_name){
  suppressWarnings(suppressMessages(require(lib_name, character.only = TRUE)))
}

import_library("logging")
import_library("tidyverse")
import_library("yaml")
import_library("complexplus")
import_library("mlflow")

# Source Utils
source("utils/exp-utils.R")


# Initial Experiment Setup -----------------------------------------------------------

basicConfig()
options(warn=-1)

exp_param_file <- "params/ready/n_qubits5__k4__n_sat3__t_step0.100000__time_T100__num_energy_levels4.yml"
# exp_param_file <- commandArgs(trailingOnly = TRUE)

# Begin our 3SAT Experiment
loginfo("Starting Experiment with conifguration: '%s'", exp_param_file)
params <- extract_params(exp_param_file)

# Setup MLflow
loginfo("Setting up MLFlow")

# Initialise MLFlow Client
loginfo("Initialising MLFlow with tracking URI: '%s'", params$experiment$`tracking-uri`)
mlflow_set_tracking_uri(params$experiment$`tracking-uri`)

# Get commit hash
mlflow_run_commit_hash <- system('git rev-parse --verify HEAD', intern = T)
loginfo("MLFlow is running with commit hash: '%s'", mlflow_run_commit_hash)

# Check if experiment already  present (if not create one)
if (!(params$experiment$name %in% mlflow:::mlflow_list_experiments()$name)) {
  logwarn("Experiment %s not found, creating...", params$experiment$name)
  mlflow::mlflow_create_experiment(name = params$experiment$name)
  mlflow_set_experiment(params$experiment$name)
} else {
  loginfo("Experiment %s already present, running run with configuration %s", params$experiment$name, exp_param_file)
  mlflow_set_experiment(params$experiment$name)
  
}

# Start mlflow run
loginfo("Starting Run")
with(mlflow_start_run(), {
  
  # browser()
  
  loginfo("Logging parameter file: '%s'", exp_param_file)
  mlflow_log_artifact(exp_param_file)
  mlflow_log_param("file_name", exp_param_file)
  mlflow_log_param("commit_hash", mlflow_run_commit_hash)
  
  # Source in all scripts
  source_exp_scripts(params)
  source_exp_utils(params)
  
  # Set Seed
  loginfo("Setting Experiments Random Seed to %s", params$experiment$seed)
  set.seed(params$experiment$seed)
  
  # Generate Clauses --------------------------------------------------------
  
  loginfo("Generating upto 'k=%s' clauses", params$initialise$params$k)
  
  # Parameters for clause
  clause_params <- params$initialise$params %>% 
    flatten() %>% 
    as_data_frame() %>% 
    gather(key, value)
  
  # Logging clause parameters
  mlflow_log_batch(params = clause_params)
  
  # Setting system up
  d_clauses <- generate_clauses(params$initialise$params)
  
  
  # Generate Time Evolution -------------------------------------------------
  
  loginfo("Clauses generated, setting up time evolution system")
  
  quantum_params <- params$build_hamiltonians$params %>% 
    flatten() %>% 
    as_data_frame() %>% 
    gather(key, value)
  
  loginfo("Logging Quantum System Parameters")
  mlflow_log_batch(params = quantum_params)
  
  d_hamils <- generate_time_evolving_system(d_clauses, params$build_hamiltonians$params)
  
  
  # Solving Schrödingers Equation -------------------------------------------
  
  loginfo("Time Evolution of the System Built, Solving Schrödingers Equation")
  
  d_solved_system <- evolve_quantum_system(d_hamils, params$build_hamiltonians$params)
  
  
  # Extract final state
  phi_T <- d_solved_system[[nrow(d_solved_system), "phi_t"]]

  loginfo("Solved system for T='%s'", params$build_hamiltonians$params$time_T)
  
  # Generate PDF ------------------------------------------------------------
  
  state_pdf <- generate_pdf(phi_T)
  loginfo("Generating PDF across amplitudes")

  
  # Plotting PDF ------------------------------------------------------------
  
  loginfo("Plotting PDF object")
  p_state_pdf <- state_pdf %>% 
    plot_state_pdf()
  
  ggsave("tmp/final_state_vector_plot.png")
  
  mlflow_log_artifact("tmp/final_state_vector_plot.png")
  
  
  # Plotting Energy Gap -----------------------------------------------------
  
  loginfo("Plotting Energy Gap")
  p_energy_gap <- d_hamils %>% 
    plot_energy_gap()
  
  ggsave("tmp/energy_plot.png")
  mlflow_log_artifact("tmp/energy_plot.png")
  
  
  # Plotting Entanglement ---------------------------------------------------

  loginfo("Plotting Entanglement")
  
  p_entanglement <- d_solved_system %>% 
    select(time, shannon_entropy) %>% 
    plot_entanglement()

  ggsave("tmp/entanglement_plot.png")
  mlflow_log_artifact("tmp/entanglement_plot.png")
  
  
  # Minimum Energy Gap ------------------------------------------------------
  
  # TODO: Chat to charles about what the best metric for this is!!
  #' 
  # Calculate min energy gap
  min_gap <- d_hamils %>% 
    mutate(gap = n_2 - n_1) %>% 
    summarise(min = min(gap)) %>% 
    pull(min)
  

  mlflow_log_metric("min_energy_gap", min_gap)
  
  
  # Ending Experiment -------------------------------------------------------
  loginfo("Experiment Complete!")
  
})


