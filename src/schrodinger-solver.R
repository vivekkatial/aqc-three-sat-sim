###############################################################################
# Schrodinger Equation Solver
#
# Created Date: Wed Sep 11 10:26:40 2019
# Author: Vivek Katial
###############################################################################

# Solve Schrodingers Equation Analytically --------------------------------

#' This function solves schrodingers equation analytically for a small time step, t
#' The function returns a state vector corresponding to the state of phi
#' @param H_b A dataframe consisting of Initial Hamiltonian
#' @param H_p A dataframe consisting of Final Hamiltonian
#' @param params A list containing `n_qubits` and `t_step`
#' @param t The time at which we're solving schrodingers equation
#' @param phi_init A state vector to use as the initial state vector
#' 
#' @return A state vector corresponding to the state of phi
#' @export
#' 
#' @example TBC...
solve_schrodinger_analytically = function(H_b, H_p, params, t, phi_init){
  
  H_1 = build_ham(H_b, H_p, t, params$time_T)
  H_2 = build_ham(H_b, H_p, t + params$t_step, params$time_T)
  
  # Get current hamiltonian
  H_curr = (H_1 + H_2)/2
  
  
  # Compute iHt
  ham_p <- -1i* H_curr*params$t_step
  
  # Calculate the state vector by e^A phi
  state_vec <- matexp(ham_p) %*% phi_init
  
  # Find normalisation factor
  norm_factor <- sqrt(sum((state_vec * Conj(state_vec))))
  
  # Normalise state vector
  state_vec_new <- state_vec/norm_factor
  
  # Validate the normalisation
  if (Re(sum(state_vec_new*Conj(state_vec_new))) - 1 < 1e-13) {
    print(sprintf("Solved at t=%s now solving eigenvalue problem",t))
  } else {
    stop("Vector not normalised")
  }
  
  eigen_sol <- get_H_curr_eigen(H_curr = H_curr, ret.system = F, params)
  
  # Extract energies
  l_energy <- eigen_sol$eigen_values
  
  # Output results
  list(
    state_vector = state_vec_new,
    l_energy = l_energy
  )
  
}

# Convert state vector to PDF ---------------------------------------------

#' This function takes a state vector and converts it into a probability distribution of the amplitudes
#' @phi A matrix with each element as a complex class
#' @return A tibble corresponding to a pdf
generate_pdf = function(phi){
  
  # Create probability distribution
  pdf <- (phi * Conj(phi)) %>% 
    as_data_frame() %>% 
    rename(p = 1) %>% 
    mutate(p = Re(p)) %>% 
    mutate(state = 0:(n()-1)) %>% 
    as_tibble() %>% 
    mutate(
      bit_str = map_chr(state, convert_int_to_bit),
      bit_str = str_remove_all(bit_str, "\\..*"),
      bit_str = str_pad(bit_str, side = "left", pad = "0", width = log2(length(phi)))
    )
  
  pdf
}



# Function to build temporary Hamiltonian ---------------------------------
#' Determine the current hamiltonian in an evolution from adiabatic theorem
#' @param H_b
#' @param H_p
#' @param t_step
#' @param run_time
build_ham = function(H_b, H_p, t_step, run_time){
  # Build current Hamiltonian
  H_curr = (1 - t_step/run_time)*H_b + (t_step/run_time)*H_p
}

