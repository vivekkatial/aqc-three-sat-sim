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

  
  # Unpack parameters
  params = list(...)[[1]]
  
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
  
  for (i in 1:params$num_energy_levs) {
    
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



# OLD STUFF ---------------------------------------------------------------

# Define Pauli X matrix
# X = matrix(c(0,1,1,0), nrow = 2, ncol = 2)
# Had = 1/sqrt(2) * matrix(c(1,1,1,-1),nrow=2,ncol=2)
# 
# 
# # Define the initial hamiltonion given by 0.5*(I - \sigma_x)
# H_init = 0.5*(diag(1,2,2)- X)
# 
# H_init = matrix(c(0,0.1,0.1,1.0), nrow = 2, ncol = 2)
# 
# # Define the Problem Matrix
# H_p = matrix(c(1,0,0,0), nrow = 2, ncol = 2)
# 
# d_time <- tibble(
#   s = seq(0, 1, by = 0.01)
# )
# 
# find_state = function(s_t, type){
#   
#   # Construct Linear Combination
#   H_s = (1 - s_t)*H_b + s_t*H_p 
#   
#   # Eigenvalue
#   eigen_info = H_s %>% eigen()
#   
#   # Extract eigenvalue
#   if (type == "ground") {
#     eigen_info$values %>% sort() %>% nth(1)
#   } else if (type == "excited") {
#     eigen_info$values %>% sort() %>% nth(2)
#   }
# }
# 
# 
# X_state = X %x% diag(2)
# 
# #H_b = (0.5*(diag(2*2) - X_state))*2
# 
# 
# # 
# # d_time %>% 
# #   mutate(
# #     ground = map2_dbl(s, "ground", find_state),
# #     excited = map2_dbl(s, "excited", find_state)
# #   ) %>% 
# #   gather(state, val, -s) %>% 
# #   ggplot(aes(x = s, y = val, col = state)) +
# #   geom_line() + 
# #   theme_minimal()
# # 
# # 
# # d_time
# 
