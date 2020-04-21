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
  
  # Return values
  if (ret.system == TRUE) {
    list(
      H_eigen = H_eigen,
      eigen_values = l_eigen
    )
  } else {
    list(
      eigen_values = l_eigen
    )
  }
}
