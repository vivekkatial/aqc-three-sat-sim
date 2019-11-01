###############################################################################
# This evolves the quantum  system according to schordingers equation
#
# Author: Vivek Katial
# Created 2019-10-31 00:11:52
###############################################################################

#' This function evolves the system
#' @param d_hamils A dataframe consisting of the valid  hamiltonina
#' @param ... A list consisting of the  number of qubits, timestep
#' @return A state vector phi with  the amplitudes of being in  each quantum state
evolve_quantum_system = function(d_hamils, ...){
  
  # Unpack parameters
  params = list(...)[[1]]
  
  # Make params numeric
  params = lapply(params, as.numeric)
  
  # browser()
  
  for (t in seq(0, params$time_T, by = params$t_step)) {
    
    # Evaluate first initial step
    if (t == 0) {
      # Initialise ground state vector
      
      loginfo("Initialising State Vector")
      phi_0 <- rep(1/(2^(params$n_qubits/2)), (2^params$n_qubits))
      
    } else if (t == (0 + params$t_step)) {
      # Solve first time step
      phi_T <- solve_schrodinger_analytically(
        d_hamils,
        params,
        t,
        phi_0
      )
      
    } else {
      # Solve subsequent systems
      phi_T <- solve_schrodinger_analytically(
        d_hamils,
        params,
        t,
        phi_T
      )
    }
  }
  
  phi_T
}


#' This function evaluates the entanglement in the system at all states
#' @param d_hamils
#' @param ... List options
#' @return A dataframe consisting of the entanglement in the system
calculate_system_entanglement = function(d_hamils, ...){
  browser()
}

#' This function evaluates the entanglement on a given Hamiltonian
#' @param hamiltonian The hamiltonian
#' @return A value for entanglement
calculate_entanglement = function(hamiltonian){
  
}