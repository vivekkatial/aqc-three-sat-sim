###############################################################################
# These R scripts construct Hamiltonian Matrices
#
# Created Date: Thu Aug 22 16:52:17 2019
# Author: Vivek Katial
############################################################################### 


# Define Matrices ---------------------------------------------------------

# Define Pauli X matrix
X = matrix(c(0,1,1,0), nrow = 2, ncol = 2)

# Define the initial hamiltonion given by 0.5*(I - \sigma_x)
H_init = 0.5*(diag(1,2,2)- X)

H_init = matrix(c(0,0.1,0.1,1), nrow = 2, ncol = 2)

# Define the Problem Matrix
H_p = matrix(c(1,0,0,0), nrow = 2, ncol = 2)

#' Sample 2x2 matrix
H_p_2x2 = matrix(c(
1,0,0,0,
0,0,0,0,
0,0,0,0,
0,0,0,1
), ncol = 4, nrow = 4)

d_time <- tibble(
  s = seq(0, 1, by = 0.01)
)

find_state = function(s_t, n){
  
  #browser()
  # Construct Linear Combination
  H_s = (1 - s_t)*H_init + s_t*H_p 
  
  # Eigenvalue
  eigen_info = H_s %>% eigen()
  
  # Extract eigen value
  eigen_info$values %>% sort() %>% nth(n)  

}


H_init = ((-(X %x% diag(2) +  diag(2) %x% X))/2 + diag(2^2))
H_p = H_p_2x2


d_time %>% 
  mutate(
    ground = map2_dbl(s, 1, find_state),
    n_2 = map2_dbl(s, 2, find_state),
    n_3 = map2_dbl(s, 3, find_state),
    n_4 = map2_dbl(s, 4, find_state)
  ) %>% 
  gather(state, val, -s) %>% 
  ggplot(aes(x = s, y = val, col = state)) +
  geom_line() + 
  theme_classic()
