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

# Source Utils
source("utils/exp-utils.R")


# Initial Setup -----------------------------------------------------------

basicConfig()

exp_param_file <- "params/ready/n_qubits5__k4__n_sat3__t_step0.100000__time_T10__num_energy_levels4.yml"
#exp_param_file <- commandArgs(trailingOnly = TRUE)

# Begin our 3SAT Experiment
loginfo("Starting Experiment with conifguration: '%s'", exp_param_file)
params <- extract_params(exp_param_file)

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

loginfo("Time Evolution Setup Complete, Plotting Energy Gap")

d_hamils %>% 
  select(-hamiltonian) %>% 
  gather(var,n, -t) %>% 
  ggplot(aes(x = t, y = n, col = var)) + 
  geom_line() + 
  theme_classic()

# Solving Schrödingers Equation -------------------------------------------

loginfo("Time Evolution of the System Built, Solving Schrödingers Equation")
phi_T <- evolve_quantum_system(d_hamils, params$build_hamiltonians$params)

loginfo("Solved system for T='%s'", params$build_hamiltonians$params$time_T)
loginfo("Generating PDF across amplitudes")


# Generate PDF ------------------------------------------------------------

state_pdf <- generate_pdf(phi_T)
state_pdf


# Plotting PDF ------------------------------------------------------------

p_state_pdf <- state_pdf %>% 
  mutate(type = ifelse(abs(p - max(p)) < 1e-13, "max", "other")) %>%
  ggplot(aes(x = bit_str, y = p, group = 1, fill = type)) +
  geom_col(alpha = 0.6) +
  labs(
    x = "State "
  ) +
  theme_classic() +
  #stat_smooth(geom = "area", span = 0.4, method = "glm", alpha = 0.4) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


# Print H_b and H_p -------------------------------------------------------

loginfo("Outputting Problem Hamiltonian")

d_hamils %>% 
  slice(n()) %>% 
  pull(hamiltonian) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(
    ind = 0:(n()-1),
    bin = map_dbl(ind, convert_int_to_bit)
    ) %>% 
  rename_all(funs(str_replace(., "X", "z_")))

loginfo("Experiment Complete")


h_0_decomp <- d_hamils %>% 
  slice(n()) %>% 
  pull(hamiltonian) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  eigen()

loginfo("Satisfying Argument as follows:")

h_0_decomp$vectors %>% 
  as_tibble() %>% 
  select(assignment = length(.)) %>% 
  mutate(
    index = paste0("z_", 1:n())
  ) %>% 
  select(index, assignment)
