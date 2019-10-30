###############################################################################
# These R scripts construct Hamiltonian Matrices
#
# Created Date: Thu Aug 22 16:52:17 2019
# Author: Vivek Katial
############################################################################### 


# Source Relevant Scripts -------------------------------------------------

source("src/build-hamiltonians/generate-initial-ham.R")
source("src/build-hamiltonians/generate-problem-ham.R")


# Define Matrices ---------------------------------------------------------

#' Function to generate the time evolving system
generate_time_evolving_system = function(d_clauses, ...){

  # browser()  
  # Unpack parameters
  params = list(...)[[1]]
  
  # Conver parameters to be numeric
  params = lapply(params, as.numeric)
  
  # Build problem hamiltonian
  H_p = construct_ham_problem(params$n_qubits, d_clauses)
  
  H_b = create_ham_initial(params$n_qubits, d_clauses)
  
  # Build initial hamiltonian
  # H_b = create_h_b_ind(params$n_qubits, d_clauses$k_1)
  
  time_vector <- seq(0, params$time_T, by = params$t_step)
  
  d_system <- tibble(
    t = time_vector
  ) %>% 
    # Add the hamiltonian as a new col
    mutate(
      hamiltonian = map(t, function(t) {H_b*(1-(t/params$time_T)) + H_p*(t/params$time_T)})
    )
  
  for (i in 1:params$num_energy_levels) {
    
    # Construct col names
    var <- paste0("n_", i)
    
    d_system <- d_system %>% 
      # Add new column
      mutate(!!var := map2_dbl(hamiltonian, i, get_eigen))
    
  }
  
  
  return(d_system)
}

#' Given a hamiltonian and required energy state find the energy of the n'th state
#' @param hamiltonian A 2^n x 2^n matrix
#' @param n Number of qubits
get_eigen = function(hamiltionian, n){
  
  # Solve the eigenvalue problem for H
  eigen_info = hamiltionian %>% 
    eigen()
  
  # Extract the n'th eigen value
  eigen_val = eigen_info$values %>% 
    sort() %>% 
    nth(n=n)
  
}