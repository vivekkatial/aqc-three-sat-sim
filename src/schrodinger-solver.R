###############################################################################
# Schrodinger Equation Solver
#
# Created Date: Wed Sep 11 10:26:40 2019
# Author: Vivek Katial
###############################################################################

# Solve Schrodingers Equation Analytically --------------------------------

#' This function solves schrodingers equation analytically for a small time step, t
#' The function returns a state vector corresponding to the state of phi
#' @param d_hamiltonian A dataframe consisting of nested hamiltonians as elements
#' @param params A list containing `n_qubits` and `t_step`
#' @param t The time at which we're solving schrodingers equation
#' @param phi_init A state vector to use as the initial state vector
#' 
#' @return A state vector corresponding to the state of phi
#' @export
#' 
#' @example TBC...
solve_schrodinger_analytically = function(d_hamiltonian, params, t, phi_init){
  
  # TODO: CHECK PARAMS
  # browser()
  # Find which hamiltonian to select
  # if (t > 0.9) {
  #   browser()
  # }
  
  t_ind <-  which(abs(d_hamiltonian$t - t) < 1e-12)
  
  if (sum(abs(d_hamiltonian$t - t) < 1e-12) != 1) {
    logerror("Failed because time step not")
  }
  
  ham_t <- d_hamiltonian %>%
    # take hamiltonians at t_1 and t_2
    slice((t_ind-1):t_ind) %>% 
    pull(hamiltonian) %>% 
    # convert to list
    as.list() %>% 
    # sum and divide by 2
    Reduce("+",.)/2
  
  # Compute iHt
  ham_p <- -1i* ham_t*params$t_step
  
  # Calculate the state vector by e^A phi
  state_vec <- matexp(ham_p) %*% phi_init
  
  # Find normalisation factor
  norm_factor <- sqrt(sum((state_vec * Conj(state_vec))))
  
  # Normalise state vector
  state_vec_new <- state_vec/norm_factor
  
  # Validate the normalisation
  if (Re(sum(state_vec_new*Conj(state_vec_new))) - 1 < 1e-13) {
    print(sprintf("Solved at t=%s",t))
  } else {
    stop("Vector not normalised")
  }
  
  state_vec_new
}

# Convert state vector to PDF ---------------------------------------------

#' This function takes a state vector and converts it into a probability distribution of the amplitudes
#' @phi A matrix with each element as a complex class
#' @return A tibble corresponding to a pdf
generate_pdf = function(phi){
  # browser()
  # Create probability distribution
  pdf <- (phi * Conj(phi)) %>% 
    as_data_frame() %>% 
    rename(p = 1) %>% 
    mutate(p = Re(p)) %>% 
    mutate(state = 0:(n()-1)) %>% 
    as_tibble() %>% 
    mutate(bit_str = map_chr(state, convert_int_to_bit)) %>% 
    # Padd to be a "n_qubit" system
    mutate(bit_str = str_pad(bit_str, side = "left", pad = "0", width = log(n(), base = 2)))
  
  pdf
}


# Example Run through -----------------------------------------------------

# Source scripts
# source("src/build-hamiltonians/construct-time-evolution.R")
# source("utils/exp-utils.R")
# source("utils/define-pauli-matrices.R")
# 
# # 1. Setup parameters and variables of our system
# params = list(
#   n_qubits = 5, 
#   t_step = 0.01, 
#   num_energy_levs = 2
# )
# 
# # Set clauses
# clause = list(
#   c_1 = c(1,2,3,4,5)
# )
# 
# # 2. Build all hamiltonians for all time steps 't'
# d_hamiltonians <- generate_time_evolving_system(clause, params)
# 
# # 3. Evolve system by solving schorodingers equation
# for (t in seq(0,1, by = params$t_step)) {
#   
#   # Evaluate first initial step
#   if (t == 0) {
#     # Initialise ground state vector
#     
#     phi_0 <- rep(1/(2^(params$n_qubits/2)), (2^params$n_qubits)) 
# 
#   } else if (t == (0 + params$t_step)) {
#     # Solve first time step
#     phi_T <- solve_schrodinger_analytically(
#       d_hamiltonians,
#       params,
#       t,
#       phi_0
#     )
#     
#   } else {
#     # Solve subsequent systems
#     phi_T <- solve_schrodinger_analytically(
#       d_hamiltonians,
#       params,
#       t,
#       phi_T
#     )
#   }
# }
# 
# phi_T %>% 
#   generate_pdf() %>% 
#   mutate(type = ifelse(abs(p - max(p)) < 1e-13, "max", "other")) %>% 
#   ggplot(aes(x = bit_str, y = p, group = 1, fill = type)) + 
#   geom_col(alpha = 0.6) + 
#   labs(
#     x = "State "
#   ) + 
#   theme_classic() + 
#   #stat_smooth(geom = "area", span = 0.4, method = "glm", alpha = 0.4) +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1)
#   )