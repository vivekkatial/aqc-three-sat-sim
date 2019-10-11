###############################################################################
# This script is built to generate test instances of 3-SAT for varying
# sizes of qubits
#
# Created Date: Thu Aug 22 15:28:14 2019
# Author: Vivek Katial
###############################################################################


create_instance_space = function(n_qubits){
  
  # Create qubits
  qubits = lapply(
    1:n_qubits, function(x)
      c(0,1)
  )
  
  # Create names for index
  ind_names = paste("z", 1:n_qubits, sep = "_")
  
  # Build set of instances
  d_instance_space <- qubits %>% 
    # Create columns 
    setNames(ind_names) %>% 
    # Build states
    cross_df() %>% 
    # Add column for binary string
    unite(bin_str, 1:ncol(.),sep="", remove = F) %>% 
    # Convert to decimal
    mutate(num = strtoi(bin_str, base=2)) %>% 
    # Arrange by decimal number
    arrange(num) %>% 
    select(num, bin_str, everything())
  
  d_instance_space
}

#' This function finds the satisfying number of arguments for a given clause on an n-bit string
#'
#'
find_num_assignments = function(d_instances, test_clause){
  
}

generate_clauses = function(...){
  
  params <- list(...)[[1]]
  
  n_qubits = params$n_qubits
  k = params$k
  
  tryCatch(
    if (k>n_qubits) {
      stop("")
    },
    error = function(e){
      logerror("Number of clauses 'k:= %s' is higher than 'N:= %s' The number of Qubits", k, n_qubits)
    }
  )
  
  clauses = list()
  
  for (i in 1:k) {
    
    clause_i = sample(1:n_qubits, params$n_sat) %>% sort()
    
    # Check that the clause exists
    if (clauses %>% has_element(clause_i) == FALSE) {
      
      
      #' TODO: Write a function which will check whether or not a valid clause
      #' 1. Calculate number of satisfying arguments
      #' 2. Check whether satisfying number of assignments = 0
      #' 3. If satisfying number of assignments > 0, remove clause
      
      # Add new clause to the list of clauses
      clauses[[paste0("k_", i)]] = clause_i
    }
  }
  
  clauses
}