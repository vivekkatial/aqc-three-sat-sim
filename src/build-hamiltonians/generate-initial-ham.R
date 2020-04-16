###############################################################################
# This script generates the initial hamiltonian
#
# Author: Vivek Katial
# Created 2019-08-26 22:42:37
###############################################################################

#' This function generates the initial Hamiltonian for a problem
#' This is given by Farhi's paper
#' @param n_qubits The number of qubits in the system
#' @param d_clauses A `list` containing the clauses on the n-bit strings
create_ham_initial = function(n_qubits, d_clauses){
  
  if (!is.numeric(n_qubits)) {
    n_qubits = as.numeric(n_qubits)
  }
  
  # Across all clauses find out how many times each bit occurs
  H_b = lapply(1:n_qubits, function(x){
      create_h_b_ind(n_qubits = n_qubits, bit = x)
  }) %>% 
    Reduce('+', .)
  
  # Return initial Hamiltonian
  H_b
}



#' This function generates H_b for a given bit in an n-bit string
#' @param n_qubits Number of qubits in n_qubit string
#' @param bit Index of bit we are working on
create_h_b_ind = function(n_qubits, bit, ...){
  
  # Initialise 
  mat = 1
  
  # Loop through each bit
  for (i in 1:n_qubits) {
    # If acting on specific bit (use X)
    if (i == bit) {
      mat = mat %x% X
    } else {
    # If not specific bit leave the same
      mat = mat %x% diag(2)
    }
  }
  
  # Normalise
  final_out = (diag(2^n_qubits) - mat )*0.5
  
  # Output final
  final_out
}