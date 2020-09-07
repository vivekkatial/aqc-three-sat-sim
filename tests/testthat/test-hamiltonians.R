###############################################################################
# This script has hamiltonian tests
#
# Author: Vivek Katial
# Created 2020-04-02 14:12:45
###############################################################################

#' This function validates that the problem hamiltonian has been constructed correctly
#' @param ham This is an r `matrix` object
#' @param experiment_name Name of experiment
test_problem_ham = function(ham, experiment_name){

  # If USA instance validate USA.
  if (str_detect(experiment_name, "usa")) {
    test_usa_ham(ham)
  }
  
  # Return statement if no tests present
  else{
    logwarn(sprintf("No tests written for '%s'", experiment_name))
  }
}

# This function validates that a USA instance has been correctly built
#' @param ham Hamiltonain matrix
test_usa_ham = function(ham){
  
  # Verify that minimum element is 0
  tryCatch(
    testthat::expect_equal(min(diag(ham)), 0),
    error = function(e){
      stop(sprintf("Minimum value in Hamiltonian is: '%s', expected 0", min(diag(ham))))
    }
  )
  
  # Verify that at most 1 element is 0
  tryCatch(
    testthat::expect_equal(sum(diag(ham)==0), 1),
    error = function(e){
      stop(sprintf("Number of satisfying assignments is: '%s', expected 0", sum(diag(ham)==0)))
    }
  )
  
  return(0)
}
