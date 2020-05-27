###############################################################################
# UI Elements for results application
#
# Author: Vivek Katial
# Created 2020-05-26 01:07:02
###############################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Update y axis
  observe({
    x <- input$response
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    d_choices <- d_runs %>% 
      names() %>% 
      tibble(name = .) %>% 
      filter(str_detect(name, "metrics|param")) %>% 
      filter(!str_detect(name, x)) %>% 
      pull()
    
    # Can also set the label and select items
    updateSelectInput(session, "feature",
                      label = "Feature",
                      choices = d_choices,
                      selected = d_choices[1]
    )
  })
   
  output$distPlot <- renderPlot({
    
    d_runs %>% 
      select(input$feature, input$response, input$group) %>% 
      mutate(!! input$group := as.factor(!!as.symbol(input$group))) %>% 
      ggplot(aes_string(x = input$feature, y = input$response, col = input$group)) + 
      geom_point() +
      theme_minimal()
    
  })
  
})
