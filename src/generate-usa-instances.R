###############################################################################
# Generating USA Instances for 3SAT (@Farhi2001)
#
# Created Date: Mon Mar 16 11:29:23 2020
# Author: Vivek Katial
###############################################################################

#' Generates USA clauses based on `n_qubits` and `k`
#' @param `list(n_qubits = N, n_sat = k)` Where N as an `int` which represents the the number of qubits and `k` number of literals in each SAT clause
#' @example
#' list(n_qubits = 13, n_sat=3) %>% 
#'  generate_usa_clauses()
generate_usa_clauses = function(...){
  # Extract params
  params <- list(...)[[1]]
  
  # Extract number of qubits
  n_qubits = params$n_qubits
  
  # Inititate instance space
  d_instances <- .create_instance_space(n_qubits)
  
  # Initiate arbitarty variable (2^n) to find number of satisfying assignments  
  num_sat_assignments = 2^n_qubits
  # Clause ticker
  i = 2
  # Set initial clause
  clause_init = sample(1:n_qubits, params$n_sat) %>% sort()
  # Initiate datastructure to hold clauses
  clauses = list(k_1 = clause_init)
  
  # Pick 3 random bits
  d_filtered_instances = .calc_satisfying_assignments(d_instances, clause_init)
  
  while(num_sat_assignments != 1){
    
    clause_i = sample(1:n_qubits, params$n_sat) %>% sort()
    
    
    # Check that the clause exists
    if (clauses %>% has_element(clause_i) == FALSE) {
      
      # Filter to only new instances
      d_filtered_instances = .calc_satisfying_assignments(d_filtered_instances, clause_i)

      # Find new number of satisfying assignments
      new_num_sat_assignments = nrow(d_filtered_instances)
      
      if (new_num_sat_assignments < num_sat_assignments & new_num_sat_assignments > 0) {
        # Adding clause
        # print(sprintf("Adding clause %s", clause_i))
        # Add new clause to the list of clauses
        clauses[[paste0("k_", i)]] = clause_i
        num_sat_assignments = new_num_sat_assignments
      }
      # Increment clause ticker
      i = i + 1
    }
  }
  
  clauses
}

#' This function calculates the number of satisfying assignments for a set of clause 
#' on a given instance space.
#' @param d_instances `tibble` Corresponding to the current instance space
#' @param clause `list()` A vector 
.calc_satisfying_assignments = function(d_instances, clause){
  
  # Create a vector for column names
  clause = paste0("z_", clause)
  
  # Loop through each clause variable and map to a column
  for (i in 1:length(clause)) {
    
    #  Assign clause variable i to the correct column
    clause_bit = clause[i]
    clause_var = paste0("clause_",i)
    assign(clause_var, clause_bit)
    
    rm(clause_bit)
  }
  
  # Currently only applies for 3SAT (filter on correct rows)
  d_filtered_instances <- d_instances %>% 
    filter(
      (!!rlang::sym(clause_1) +!!rlang::sym(clause_2)  +!!rlang::sym(clause_3)) == 1
    )
  
  # Num satisfying assignments
  num_satisf = nrow(d_filtered_instances)
  
  if (num_satisf == 0) {
    d_instances
  } else {
    # Return filtered dataframe and num_satisfying assignments
    d_filtered_instances  
  }
}


#' This function generates an instance space of satisfying assignments 
#' for the clauses
#' @param n_qubits
#' @return d_instance space
.create_instance_space = function(n_qubits){
  
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
