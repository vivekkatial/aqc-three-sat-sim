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

#  Initialise MLFlow Client
loginfo("Initialising MLFlow with tracking URI: '%s'", params$experiment$`tracking-uri`)
mlflow_set_tracking_uri(params$experiment$`tracking-uri`)
mlflow_client <- mlflow:::mlflow_client(tracking_uri = params$experiment$`tracking-uri`)

# Check if experiment already  present (if not create one)
if (!(params$experiment$name %in% mlflow:::mlflow_list_experiments()$name)) {
  logwarn("Experiment %s not found, creating...", params$experiment$name)
  mlflow::mlflow_create_experiment(params$experiment$name, params$experiment$name)
} else {
  loginfo("Experiment %s already present, running run with configuration %s", params$experiment$name, exp_param_file)
}




# Start mlflow run
mlflow_start_run()

mlflow_log_artifact(exp_param_file)
mlflow_log_param("file_name", exp_param_file)

# Source in all scripts
source_exp_scripts(params)
source_exp_utils(params)

# Set Seed
loginfo("Setting Experiments Random Seed to %s", params$experiment$seed)
set.seed(params$experiment$seed)

# Generate Clauses --------------------------------------------------------

loginfo("Generating upto 'k=%s' clauses", params$initialise$params$k)

# Setting system up
d_clauses <- generate_clauses(params$initialise$params)


# Generate Time Evolution -------------------------------------------------

loginfo("Clauses generated, setting up time evolution system")

d_hamils <- generate_time_evolving_system(d_clauses, params$build_hamiltonians$params)


# Solving Schrödingers Equation -------------------------------------------

loginfo("Time Evolution of the System Built, Solving Schrödingers Equation")
phi_T <- evolve_quantum_system(d_hamils, params$build_hamiltonians$params)

loginfo("Solved system for T='%s'", params$build_hamiltonians$params$time_T)

# Solving for Entanglement
# d_entanglement <- calculate_system_entanglement(d_hamils)


# Generate PDF ------------------------------------------------------------

loginfo("Generating PDF across amplitudes")
state_pdf <- generate_pdf(phi_T)

# Plotting PDF ------------------------------------------------------------

loginfo("Plotting PDF object")
p_state_pdf <- state_pdf %>% 
  plot_state_pdf()


# Plotting Energy Gap -----------------------------------------------------

loginfo("Plotting Energy Gap")
p_energy_gap <- d_hamils %>% 
  plot_energy_gap()


# Ending Experiment -------------------------------------------------------

# Ending run
mlflow_end_run()
loginfo("Experiment Complete!")

