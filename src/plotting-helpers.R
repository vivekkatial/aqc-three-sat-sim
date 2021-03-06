###############################################################################
# Scripts to produce plotting artifacts from one main run
#
# Created Date: Thu Oct 31 09:42:59 2019
# Author: Vivek Katial
###############################################################################

#' This function plots the energy gap for a hamiltonian dataframe
#' @param d_solved_system This is a dataframe which consists of the hamiltonian matrix in each column
#' @return A ggplot object containing a plot of the energy gap over time for the system
plot_energy_gap = function(d_solved_system){

  # Check the correct data frame
  if (!all(c("time", "lambda_1", "lambda_2") %in% names(d_solved_system))) {
    stop("Incorrect data frame fed into function 'plot_energy_gap'")
  }
  
  p_energy_gap <- d_solved_system %>% 
    select(time, lambda_1, lambda_2) %>% 
    gather(var,n, -time) %>% 
    ggplot(aes(x = time, y = n, col = var)) + 
    geom_line() + 
    theme_classic() + 
    labs(
      x = "time",
      y = "energy"
    )

}

#' This function plots the state vector as a probability distribution
#' @param state_pdf The state vector as a probability distribution ('type' = tibble)
#' @return A ggplot object containing a plot of the energy gap over time for the system
plot_state_pdf = function(state_pdf){
  
  if (!all(c("p", "state", "bit_str") %in% names(state_pdf))) {
    stop(sprintf("Column(s) '%s' should not be in 'state_pdf'", diff(names(state_pdf), c("p", "state", "bit_str"))))
  }
  
  state_pdf %>% 
    mutate(type = ifelse(abs(p - max(p)) < 1e-13, "max", "other")) %>%
    ggplot(aes(x = bit_str, y = p, group = 1, fill = type)) +
    geom_col(alpha = 0.6) +
    labs(
      x = "State "
    ) +
    theme_classic() +
    #stat_smooth(geom = "area", span = 0.4, method = "glm", alpha = 0.4) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}

#' This function plots the entanglemnet of a solved system
#' @param d_solved_system A solved quantum system
#' @return A ggplot object containing entanglement plot
plot_entanglement = function(d_solved_system, label){
  
  if (!(is_character(label))) {
    stop("Please make label a character")
  }
  
  # Plot Shannon Entropy at the end
  d_solved_system %>% 
    select(time, shannon_entropy) %>% 
    ggplot(aes(x = time, y = shannon_entropy)) + 
    geom_line() + 
    theme_classic() + 
    labs(
      x = "t",
      y = label
    )
}

