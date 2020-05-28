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
import_library("testthat")

# Source Utils
source("utils/exp-utils.R")


# Initial Experiment Setup -----------------------------------------------------------

basicConfig()
options(warn=-1)

# exp_param_file <- "params/ready/n_sat3__t_step0.010000__time_T100__num_energy_levels4__instance_index10.000000__n_qubits5.000000.yml"
exp_param_file <- commandArgs(trailingOnly = TRUE)

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

# Make a new tmp folder for this run
if (dir.exists("tmp/")) {
  unlink("tmp", recursive = T)
  dir.create("tmp/")
} else {
  dir.create("tmp/")
}

# Start mlflow run
loginfo("Starting Run")
with(mlflow_start_run(), {
  
  loginfo("Logging parameter file: '%s'", exp_param_file)
  mlflow_log_artifact(exp_param_file)
  mlflow_log_param("file_name", exp_param_file)
  mlflow_log_param("commit_hash", mlflow_run_commit_hash)
  
  # Source in all scripts
  source_exp_scripts(params)
  source_exp_utils(params)
  
  # Set Seed
  loginfo("Setting Experiments Random Seed to %s", params$experiment$seed)
  set.seed(params$initialise$params$instance_index)
  
  # Generate Clauses --------------------------------------------------------
  
  loginfo("Generating clauses", params$initialise$params$k)
  
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
  
  # BUILD INITIAL HAMILTONIAN (H_b)
  H_b = create_ham_initial(
    params$build_hamiltonians$params$n_qubits
    )
  
  # BUILD PROBLEM HAMILTONIAN (H_p)
  H_p = create_ham_problem(
    n_qubits = params$build_hamiltonians$params$n_qubits,
    clauses = d_clauses
  )
  
  # Verify Problem Hamiltonian
  test_problem_ham(H_p, params$experiment$name)
  
  
  # Solving Schrödingers Equation -------------------------------------------
  
  loginfo("Time Evolution of the System Built, Solving Schrödingers Equation")
  
  l_solved_system <- evolve_quantum_system(H_b, H_p, params$build_hamiltonians$params)
  d_solved_system <- l_solved_system$d_solved_system
  
  
  # Extract final state
  phi_T <- l_solved_system$phi_T %>% 
    unlist()

  loginfo("Solved system for T='%s'", params$build_hamiltonians$params$time_T)
  
  # Generate PDF ------------------------------------------------------------
  
  state_pdf <- generate_pdf(phi_T)
  loginfo("Generating PDF across amplitudes")
  

  # Find Probability of Success ------------------------------------------------------------
  
  p_success <- state_pdf %>% 
    filter(bit_str == solve_three_sat(d_clauses, params$initialise$params$n_qubits)) %>% 
    pull(p)
  
  mlflow_log_metric("p_success", p_success)
  
  # Plotting Energy Gap -----------------------------------------------------
  
  loginfo("Plotting Energy Gap")
  
  p_energy_gap <- d_solved_system %>%
    plot_energy_gap()

  ggsave("tmp/energy_plot.png")
  mlflow_log_artifact("tmp/energy_plot.png")

  # Plotting State Entanglement ---------------------------------------------
  
  loginfo("Plotting State Entanglement")
  
  p_entanglement <- d_solved_system %>% 
    select(time, shannon_entropy) %>% 
    plot_entanglement(label = "State Entropy")

  ggsave("tmp/entanglement_plot.png")
  mlflow_log_artifact("tmp/entanglement_plot.png")
  

  # Plotting Ground State Entanglement --------------------------------------
  loginfo("Plotting Ground State Entanglement")
  
  p_gs_entanglement <- d_solved_system %>% 
    select(time, shannon_entropy = ground_state_entropy) %>% 
    plot_entanglement(label = "Ground State Entropy")
  
  ggsave("tmp/gs_entanglement_plot.png")
  mlflow_log_artifact("tmp/gs_entanglement_plot.png")

  # Minimum Energy Gap ------------------------------------------------------
  
  # TODO: Chat to charles about what the best metric for this is!!
  #' 
  # Calculate min energy gap
  min_gap <- d_solved_system %>% 
    mutate(gap = lambda_2 - lambda_1) %>% 
    summarise(min = min(gap)) %>% 
    pull(min)
  
  mlflow_log_metric("min_energy_gap", min_gap)
  
  # Maximum Entanglement Entropy --------------------------------------------
  
  max_shannon_entropy <- max(d_solved_system$shannon_entropy)
  mlflow_log_metric("max_shannon_entropy", max_shannon_entropy)
  
  max_ground_state_entropy <- max(d_solved_system$ground_state_entropy)
  mlflow_log_metric("max_ground_state_entropy", max_ground_state_entropy)
  
  

  # Mean Entanglement Entropy -----------------------------------------------

  mean_shannon_entropy <- mean(d_solved_system$shannon_entropy)
  mlflow_log_metric("mean_shannon_entropy", mean_shannon_entropy)
  
  mean_ground_state_entropy <- mean(d_solved_system$ground_state_entropy)
  mlflow_log_metric("mean_ground_state_entropy", mean_ground_state_entropy)
  
  # Median Entanglement Entropy ---------------------------------------------

  median_shannon_entropy <- median(d_solved_system$shannon_entropy)
  mlflow_log_metric("median_shannon_entropy", median_shannon_entropy)
  
  median_ground_state_entropy <- median(d_solved_system$ground_state_entropy)
  mlflow_log_metric("median_ground_state_entropy", median_ground_state_entropy)
  

  # Standard Deviation Entanglement Entropy ---------------------------------

  sd_shannon_entropy <- sd(d_solved_system$shannon_entropy)
  mlflow_log_metric("sd_shannon_entropy", sd_shannon_entropy)
  
  sd_ground_state_entropy <- sd(d_solved_system$ground_state_entropy)
  mlflow_log_metric("sd_ground_state_entropy",  sd_ground_state_entropy)

  # SAT - Clause/Var Ratio --------------------------------------------------
  
  clause_var_ratio <- length(d_clauses)/as.numeric(params$initialise$params$n_qubits)
  mlflow_log_metric("clause_var_ratio", clause_var_ratio)
  
  # Ending Experiment -------------------------------------------------------
  loginfo("Experiment Complete!")
  
  # Logging datafiles -------------------------------------------------------
  d_solved_system %>% 
    write_rds("tmp/d_solved_system.rds")
  
  d_clauses %>% 
    write_rds("tmp/d_clauses.rds")
  
  phi_T %>% 
    write_rds("tmp/phi_T.rds")
  
  mlflow_log_artifact("tmp/d_solved_system.rds")
  mlflow_log_artifact("tmp/d_clauses.rds")
  mlflow_log_artifact("tmp/phi_T.rds")
  
})
