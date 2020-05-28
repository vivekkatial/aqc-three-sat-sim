###############################################################################
# This evolves the quantum  system according to schordingers equation
#
# Author: Vivek Katial
# Created 2019-10-31 00:11:52
###############################################################################

#' This function evolves the system
#' @param H_b A dataframe consisting of a valid  hamiltonian for the initial
#' @param H_p A dataframe consisting of a valid hamiltonian for the final
#' @param ... A list consisting of the  number of qubits, timestep
#' @return A state vector phi with  the amplitudes of being in  each quantum state
evolve_quantum_system = function(H_b, H_p, ...){
  
  # Unpack parameters
  params = list(...)[[1]]
  
  # Make params numeric
  params = lapply(params, as.numeric)
  
  # Initialise empty DF
  d_solved_system <- tibble(
    time = numeric(),
    shannon_entropy = numeric(),
    lambda_1 = numeric(),
    lambda_2 = numeric(),
    ground_state_entropy = numeric()
  )
  
  for (t in seq(0, params$time_T, by = params$t_step)) {
    
    # Evaluate first initial step
    if (t == 0) {
      # Initialise ground state vector
      loginfo("Initialising State Vector")
      phi_0 <- rep(1/(2^(params$n_qubits/2)), (2^params$n_qubits))
      
      # Find eigen vals for initial state
      init_eigen_vals <- get_H_curr_eigen(H_b, ret.system = F, params)
      
      d_solved_system <- d_solved_system %>% 
        bind_rows(
          tibble(
            time = t,
            shannon_entropy = calculate_entanglement(phi_0, params$n_qubits), # Calculate entanglement
            lambda_1 = as.numeric(init_eigen_vals$lambda_1),
            lambda_2 = as.numeric(init_eigen_vals$lambda_2),
            ground_state_entropy = 0
          )
        )
      
    } else if (t == (0 + params$t_step)) {
      
      # Solve first time step
      sol_T <- solve_schrodinger_analytically(
        H_b,
        H_p,
        params,
        t,
        phi_0
      )
    
      # Extract results
      phi_T = sol_T$state_vector
      l_energy = sol_T$l_energy
      ground_state_entropy = sol_T$ground_state_entropy
      
      # Add to data-frame of solved system
      d_solved_system <- d_solved_system %>% 
        bind_rows(
          tibble(
            time = t,
            shannon_entropy = calculate_entanglement(phi_T, params$n_qubits), # Calculate entanglement
            lambda_1 = l_energy$n_1,
            lambda_2 = l_energy$n_2,
            ground_state_entropy = ground_state_entropy
          )
        )
      
    } else {
      # Solve subsequent systems
      sol_T <- solve_schrodinger_analytically(
        H_b,
        H_p,
        params,
        t,
        phi_T
      )
      
      # Extract results
      phi_T = sol_T$state_vector
      l_energy = sol_T$l_energy
      ground_state_entropy = sol_T$ground_state_entropy
      
      # Add to data-frame of solved system and calculate entanglement
      d_solved_system <- d_solved_system %>% 
        bind_rows(
          tibble(
            time = t,
            shannon_entropy = calculate_entanglement(phi_T, params$n_qubits), # Calculate entanglement as shannon entropy
            lambda_1 = l_energy$n_1,
            lambda_2 = l_energy$n_2,
            ground_state_entropy = sol_T$ground_state_entropy
          )
        )
    }
  }
  
  list(
    d_solved_system = d_solved_system,
    phi_T = phi_T
  )
}
