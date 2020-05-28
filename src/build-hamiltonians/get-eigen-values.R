###############################################################################
# This script extracts `n` eigenvalues from the Hamiltonian
#
# Created Date: Thu Aug 22 16:52:17 2019
# Author: Vivek Katial
############################################################################### 

#' Function to generate the time evolving system
get_H_curr_eigen = function(H_curr, ret.system = FALSE, ...){
  
  # Extract params
  params <- list(...)[[1]]
  
  # Eigen vals
  l_eigen = list()
  
  # Solve eigen system
  H_eigen <- H_curr %>% 
    eigen()
  
  for (i in 1:params$num_energy_levels) {
    
    # Construct variable
    var = paste0("n_", i)
    
    # Extract Eigenvalues
    H_eigen_vals <- H_eigen$values %>% 
      sort() %>% 
      nth(n = i)
    
    l_eigen[[var]] = H_eigen_vals
    
  }
  
  # Ground State Energy
  
  # Extract final column (this corresponds to the eigenvector for the lowest column)
  ground_state_vector = H_eigen$vectors[, 2^(params$n_qubits)]
  # Compute enrtopy of ground state
  ground_state_entropy = calculate_entanglement(.phi = ground_state_vector)
  
  
  # Return values
  if (ret.system == TRUE) {
    list(
      H_eigen = H_eigen,
      eigen_values = l_eigen,
      ground_state_entropy = ground_state_entropy
    )
  } else {
    list(
      eigen_values = l_eigen,
      ground_state_entropy = ground_state_entropy
    )
  }
}
