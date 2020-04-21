###############################################################################
# This script has helper functions to generate H(t)
#
# Author: Vivek Katial
# Created 2020-04-01 15:08:07
###############################################################################

#' @param H_b A hamiltonian matrix representing initial ham
#' @param H_p A hamiltonian matrix representing problem ham
#' @param time_T Total time for evolution
#' @param t Time within evolution
generate_time_dep_ham = function(H_b, H_p, time_T, t){
  
  # Test validity of matrix
  # test_matrices(H_b, H_p)  
  H_t = H_b*(1-(t/time_T)) + H_p*(t/time_T)
  
}


