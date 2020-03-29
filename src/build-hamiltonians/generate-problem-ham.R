###############################################################################
# Script to generate problem hamiltonion
#
# Author: Vivek Katial
# Created 2019-08-26 16:33:57
###############################################################################


#' This function generates a problem hamiltonian for a single clause
#' @param n_qubits The number of qubits needed
#' @param clause The number of clauses
construct_ham_clause = function(n_qubits, clause){
  
  # Build base Hamiltonian
  H_c_checked = diag(2^n_qubits) %>% 
    # Create Bit string for each val
    clean_hamiltonian(n_qubits) %>% 
    # Check satisfiability on clause
    mutate(
      sat = map_chr(bit_str, check_sat, clause)
    )
  
  # Change values for Hamiltonian to 0
  H_c_checked[
    which(H_c_checked$sat == T),
    which(H_c_checked$sat == T)
    ] = 0
  
  H_c = H_c_checked %>% 
    select(-c("ind", "bit_str", "sat")) %>% 
    as.matrix()
  
  colnames(H_c) <- NULL
  
  H_c
}


#' For a given set of clauses this function generates the problem hamiltonian
#' @param clauses
#' @param n_qubits
construct_ham_problem = function(n_qubits, clauses){
  
  # Across all clauses 
  H_p = lapply(clauses, function(clause){
    construct_ham_clause(n_qubits, clause)
  }) %>% 
    Reduce('+', .)
  
  # Return initial Hamiltonian
  H_p
  
}

#' This function checks whether or not a bit string satisfies a specific clause
#' The clause is represented by a vector of indexes
#' For example the vector c(1,2) corresponds to the clause z_1 + z_2 = 1
#' @param bit_string string representing the bit we're evaluating the constraint
#' @param clause A numeric vector representing the indices
check_sat = function(bit_string, clause){
  
  # Check bit string
  bit_vec = bit_string %>% 
    str_split("") %>% 
    unlist() %>% 
    rev()
  
  # Check if each bit satisfies clause assignment
  sat = lapply(clause, function(x)
    bit_vec[x] == 1
  ) %>% 
    Reduce('+', .)
  
  # Verify valid formula being presented
  if (is.na(sat)) {
    print(sprintf("Bit String: \t %s", bit_string))
    print(sprintf("Clause: \t %s", clause))
    print(sprintf("Bit Vec: \t %s", bit_vec))
  }
  
  # Only true if ONE bit satisfies
  if (sat == 1) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}


# Example -----------------------------------------------------------------
# 
# clauses = list(
#   c_1 = c(1,2,3)
# )
# 
# exp(iL*construct_ham_problem(3, clauses)*1)
