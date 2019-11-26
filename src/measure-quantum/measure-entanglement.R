###############################################################################
# This script implements the measurement for entanglement
#
# Author: Vivek Katial
# Created 2019-11-11 11:16:58
###############################################################################

#' This function builds the density matrix for a given state_vector
#' @param .phi The state vector in consideration
#' @param .n_qubits **Optional** The number of qubits of the system
#' @return state_matrix 
build_state_matrix = function(.phi, .n_qubits){
  
  # Get dimensions
  matrix_dimensions = get_dims_density_matrix(.n_qubits)
  
  # Build state matrix
  state_matrix = .phi %>% 
    matrix(nrow = matrix_dimensions$n_row, ncol = matrix_dimensions$n_col)
  
  # Return state matrix
  state_matrix
}

# Find midpoint for bipartite system
#' @param .n_qubits Parameter for number of qubits
#' @return A list containing number of cols and number of 
get_dims_density_matrix = function(.n_qubits){
  
  # Check if n_qubits odd or even
  if (.n_qubits %% 2 == 0) {
    n_row = (2^(.n_qubits))/2
    n_col = (2^(.n_qubits))/2
  } else {
    n_row = (2^.n_qubits)/2
    n_col = (2^(.n_qubits - 1))/2
  }
  
  # Return params
  list(
    n_row = n_row,
    n_col = n_col
  )
}


#' Compute Entanglement of a state vector
#' @param .phi state vector
#' @param .n_qubits Parameter for number of qubits
#' @return shannon entropy
calculate_entanglement = function(.phi, .n_qubits=NULL){
  
  .n_qubits = as.numeric(.n_qubits)
  
  if (is.null(.n_qubits)) {
    warning("Explicitly specify number of .n_qubits for a faster execution time")
    .n_qubits = log2(length(.phi))
  }
  
  # Build State Matrix for phi
  state_matrix = build_state_matrix(.phi, .n_qubits)
  
  # Apply Singular Value Decomposition on state matrix
  decomposed_system = svd(state_matrix)
  
  # Extract correlations
  p_decomp = (decomposed_system$d)^2
  
  # Find shannon entropy
  shannon_entropy = sum(-p_decomp * log2(p_decomp))
  
  shannon_entropy
}

