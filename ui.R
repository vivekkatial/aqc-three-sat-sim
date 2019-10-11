###############################################################################
# UI definition for QC Simulation shiny app
#
# Created Date: Mon Sep  2 13:02:17 2019
# Author: Vivek Katial
###############################################################################

ui = withMathJax(argonDash::argonDashPage(
  title = "n-SAT AQC Simulation",
  author = "Vivek Katial",
  sidebar = argonDash::argonDashSidebar(
    vertical = T,
    skin = "light",
    background = "white",
    size = "md",
    side = "left",
    id = "sidebar",
    brand_url = "https://vivekkatial.netlify.com",
    argonDash::argonSidebarHeader(title = "AQA Simulation"),
    argonDash::argonSidebarMenu(
      # Defining the tabs
      argonSidebarItem(
        tabName = "overview",
        icon = "bullet-list-67",
        "Overview"
      ),
      argonSidebarItem(
        tabName = "satisfiability",
        icon = "atom",
        icon_color = '#4298b5',
        "Satisfiability"
      ),
      argonSidebarItem(
        tabName = "maxcut",
        icon = "planet",
        icon_color = '#4298b5',        
        "MAXCUT"
      ),
      argonSidebarDivider(),
      br(),
      br(),
      argonSidebarItem(
        icon = 'single-02',
        icon_color = '#4298b5',
        tabName = "about_me",
        "About Me"
      )
    )
  ),
  # Define Header
  header = argonDash::argonDashHeader(
    gradient = FALSE,
    color = 'default',
    h4('Adiabatic Quantum Computing on NP-Hard Problems', style = 'color:white;text-align:center;font-size:2em;'),
    background_img = "https://ak5.picdn.net/shutterstock/videos/10733585/thumb/1.jpg",
    height = 200
  ),
  body = argonDashBody(
    argonTabItems(
      argonTabItem(
        tabName = "overview",
        argonCard(
          width = 12,
          argonH1("Overview", display = 3),
          p("This project presents an interactive simulation of adiabatic quantum computing."),
          h2("Adiabatic Quantum Computing  (AQC)"),
          p(
            "A quantum system may be in many states at one time"
          ),
          p("
            For example a single 'qubit' (possibly represented by an electron) may be in a superposition, or 
            linear combination of states
            $$\\left|\\psi\\right> = \\alpha\\left|0\\right> + \\beta\\left|1\\right>$$ 
            Where: $$\\left|0\\right> = (1,0) \\quad \\text{and} \\quad \\left|1\\right> = \\vec{\\psi} = (0,1)$$"
            ),
          p("In the above example the coefficients squared represent the probability of observing each state. 
            This system can be further generalised such for n qubits"),
          p("$$
          \\left|\\psi\\right> = \\sum_{z}c_{z} \\left|z_1\\right>\\left|z_2\\right>...\\left|z_n\\right>
            $$")
        )
      ),
      argonTabItem(
        tabName = "satisfiability",
        argonR::argonRow(
          argonR::argonCard(
            title = h3("Instance Settings"),
            width = 12,
            argonR::argonRow(
              argonR::argonColumn(
                width = 3,
                sliderInput(
                  "n_qubits",
                  "Number of Qubits",
                  min = 1,
                  max = 7,
                  value = 3
                )
              ),
              argonR::argonColumn(
                width = 3,
                sliderInput(
                  "n_sat",
                  "Select n for n-SAT",
                  min = 1,
                  max = 3,
                  value = 2
                )
              ),
              argonR::argonColumn(
                width = 3,
                sliderInput(
                  "num_clauses",
                  "Select clauses",
                  min = 1,
                  max = 7,
                  value = 3
                )
              ),
              argonR::argonColumn(
                width = 3,
                actionButton(
                  "create_clause",
                  "Generate clauses",
                  icon = icon("atom"),
                  style = "margin-top: 40px;width: 250px;max-width: 100%;"
                )
              )
            ),
            argonRow(
              argonColumn(
                width = 4,
                withMathJax(),
                tableOutput(
                  "clauses"
                )
              )
            )
          )
        ),
        argonRow(
          argonR::argonCard(
            title = h3("Simulation Settings"),
            width = 12,
            argonRow(
              argonColumn(
                width = 3,
                selectizeInput(
                  "time_step",
                  label = "Select Time Step",
                  choices = 10^(seq(-3,-1, by = 1)),
                  selected = 1e-02
                )
              ),
              argonColumn(
                width = 3,
                selectizeInput(
                  "time_T",
                  label = "Select Time T",
                  choices = 10^(seq(0,2, by = 1)),
                  selected = NULL
                )
              ),
              argonColumn(
                width = 3,
                sliderInput(
                  "time",
                  label = "Select time t/T",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  value = 0,
                  animate  = animationOptions(
                    playButton = HTML("<img src='images/play-button.png' height='20' width='20'>"), 
                    pauseButton = HTML("<img src='images/pause-button.png' height='20' width='20'>")
                  )
                )
              ),
              argonColumn(
                width = 3,
                actionButton(
                  "build_hamiltonian",
                  "Build Hamiltonians",
                  icon  = icon("atom"),
                  style = "margin-top: 40px;width: 250px;max-width: 100%;"
                )
              )
            )
          )
        ),
        argonRow(
          argonCard(
            title = h3("Results"),
            width = 12,
            argonRow(
              argonColumn(
                width = 6,
                plotlyOutput(
                  "energy_plot"
                ) %>% 
                  withSpinner(color="#0dc5c1")
              ),
              argonColumn(
                width = 6,
                plotlyOutput(
                  "state_pdf_plot"
                )
              )
            )
          )
        )
      ),
      argonTabItem(
        tabName = "maxcut",
        argonCard(
          width = 12,
          argonH1("MAXCUT", display = 3),
          p("This project presents an interactive simulation of adiabatic quantum computing."),
          h2("Adiabatic Quantum Computing  (AQC)"),
          p("AQC is a technique which may be used to solve optimisation problems"),
          div("more math here")
        )
      ),
      argonTabItem(
        tabName = "about_me",
        argonR::argonUser(
          title = 'Vivek Katial',
          url = 'http://vivekkatial.netlify.com',
          src = 'images/vivek.png',
          style = "text-align:left",
          p("This project simulates Adiabatic Quantum Computing"),
          br(),
          p("Libraries used:"),
          div(
            tags$ul(
              tags$li("complexplus: For doing advanced linear algebra operations"),
              tags$li("Argon: Beautiful and ready to use bootstrap 4 theme. I see this as a replacement for shiny builtin ui/shinydashboard which are a little bit dated. Shout out to David Granjon for creating this amazing package and being a very responsive maintainer."),
              tags$li("tidyverse: Do I even need to mention this? Hands down to the best data manipulation package of all programming languages. This is why I use R.")
            )
          )
        )
      )
    )
  )
)
)