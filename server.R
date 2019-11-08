###############################################################################
# Define the server logic of the application
#
# Created Date: Mon Sep  2 13:01:52 2019
# Author: Vivek Katial
############################################################################### 

server <- function(input, output, session){
  
  # Ensure that number of clauses is always less than the number of qubits
  observe({
    
    # specify var
    val <- input$n_qubits
    
    updateSliderInput(
      session, 
      "num_clauses", 
      value = floor(val/2),
      min = 1, 
      max = val
    )
    
    updateSliderInput(
      session, 
      "n_sat", 
      value = floor(min(3,val)/2),
      min = min(1,val), 
      max = min(3, val)
    )
  })
  
  # Ensure that play time step is consistent with min step size
  observe({
    val <- input$time_step
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "time", value = 0,
                      min = 0, max = 1, step = val)
  })
  
  
  # Update slider when selecting T  
  observe({
    val <- input$time_T
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "time", value = 0,
                      min = 0, max = input$time_T, step = input$time_step)
    
    d_hamiltonians()
  })
  
  d_instance_params <- reactive({
    
    # Eval
    if (input$create_clause) {
      instance_params = list(
        n_qubits = isolate(input$n_qubits),
        k = isolate(input$num_clauses),
        n_sat = isolate(input$n_sat)
      )
    }
    
    
    
  })
  
  d_clauses <- reactive({
    generate_clauses(d_instance_params())
  })
  
  # Display clauses generated
  output$clauses <- renderTable({
    if (input$create_clause) {
      d_clauses() %>% 
        as_data_frame() %>% 
        gather(clause, var) %>% 
        group_by(clause) %>% 
        summarise(clause_expression =  paste("z_", var, collapse = " + ", sep = "")) %>% 
        mutate(clause_expression = paste(clause_expression, "= 1"))
    }
  })
  
  # Generate Initial Hamiltonian
  d_hamiltonians <- eventReactive(input$build_hamiltonian, {
    if (input$build_hamiltonian) {
      
      params = list(
        n_qubits = input$n_qubits,
        t_step = as.numeric(input$time_step),
        num_energy_levs = min(2^(input$n_qubits),4),
        time_T = as.numeric(input$time_T)
      )
      
      d_hamils = generate_time_evolving_system(d_clauses(), params)
      
    }
  })
  
  # Produce plot
  output$energy_plot <- renderPlotly({
    validate(
      need(input$build_hamiltonian, 'Need to build Hamiltonian'),
      need(input$create_clause, '')
    )
    
    d_hamiltonians() %>% 
      select(-hamiltonian) %>% 
      gather(var,n, -t) %>% 
      ggplot(aes(x = t, y = n, col = var)) + 
      geom_line() + 
      theme_classic()
    
    ggplotly() %>% 
      config(displayModeBar = F)
  })
  
  decomposed_h_p <- reactive({
    h_0_decomp <- d_hamiltonians() %>% 
      slice(n()) %>% 
      pull(hamiltonian) %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      as.matrix() %>% 
      eigen()
  })
  
  d_solution <- reactive({
    # browser()
    decomposed_h_p()$vectors %>% 
      as_tibble() %>% 
      select(sol = length(.)) %>% 
      mutate(
        num = 0:(n()-1),
        bin_str = map_chr(num,convert_int_to_bit),
        # add leading zeros
        bin_str = str_pad(bin_str, input$n_qubits, side = "left", pad = 0)
      ) %>% 
      # filter to correct assigment
      filter(sol == 1) %>% 
      # Extract binary string
      pull(bin_str) %>% 
      # construct into something we can display
      str_split(pattern = "") %>% 
      unlist() %>%
      as_tibble() %>% 
      rename(assignment = value) %>% 
      mutate(index = paste("z_", n():1, sep = "")) %>% 
      spread(index, assignment)
  })
  
  output$assignment_solution <- renderTable(
    d_solution()
  )
  
  d_state_vector <- reactive({
    
    # Define parameeters
    params = list(
      n_qubits = input$n_qubits,
      t_step = as.numeric(input$time_step),
      num_energy_levs = min(2^(input$n_qubits),4)
    )
    
    for (t in seq(0,as.numeric(input$time), by = params$t_step)) {
      
      # browser()
      # Evaluate first initial step
      if (t == 0) {
        # Initialise ground state vector
        phi_0 <- rep(1/(2^(params$n_qubits/2)), (2^params$n_qubits))
        phi_T <- phi_0
        
      } else if (t == (0 + params$t_step)) {
        # Solve first time step
        phi_T <- solve_schrodinger_analytically(
          d_hamiltonians(),
          params,
          t,
          phi_0
        )
        
      } else {
        # Solve subsequent systems
        phi_T <- solve_schrodinger_analytically(
          d_hamiltonians(),
          params,
          t,
          phi_T
        )
      }
    }
    
    phi_T
    
  })
  
  output$state_pdf_plot <- renderPlotly({
    
    validate(
      need(input$build_hamiltonian, 'Need to build Hamiltonian'),
      need(input$create_clause, '')
    )
    
    # Plot stuff
    d_state_vector() %>% 
      # Generate PDF
      generate_pdf() %>% 
      # Select max probs
      mutate(type = ifelse(abs(p - max(p)) < 1e-13, "max", "other")) %>% 
      ggplot(aes(x = bit_str, y = p, group = 1, fill = type)) + 
      geom_col(alpha = 0.6) + 
      labs(
        x = "State "
      ) + 
      theme_classic() + 
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    
    ggplotly() %>% 
      config(displayModeBar = F)
    
  })
}
