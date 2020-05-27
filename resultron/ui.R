###############################################################################
# Server logic for results application
#
# Author: Vivek Katial
# Created 2020-05-26 01:06:45
###############################################################################

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Results Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "response",
        label = "Response",
        choices = d_runs %>% 
          names() %>% 
          tibble(name = .) %>% 
          filter(str_detect(name, "metrics")) %>% 
          pull(),
        selected = "metrics_p_success"
      ),
      selectizeInput(
        inputId = "feature",
        label = "feature",
        choices = d_runs %>% 
          names() %>% 
          tibble(name = .) %>% 
          filter(str_detect(name, "metrics|param")) %>% 
          pull()
      ),
      selectizeInput(
        inputId = "group",
        label = "Group",
        choices = c("n_sat", "t_step", "time_t")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
