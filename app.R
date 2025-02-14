# Install required packages if not already installed
# Uncomment the following lines to install the packages if needed
# install.packages("shiny")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  
  # Enable MathJax for rendering LaTeX equations
  withMathJax(),
  
  # App title
  titlePanel("HIV Prevention Cost-Effectiveness Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Efficacy slider
      sliderInput("efficacy",
                  "Efficacy of Intervention (e):",
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 1,
                  post = "%"),
      
      # Input: Incidence without intervention slider (Updated to 0-4)
      sliderInput("incidence",
                  "HIV Incidence Without Intervention (I) per 100 person-years:",
                  min = 0,
                  max = 4,
                  value = 1,
                  step = 0.1),
      
      # Input: Duration of protection slider
      sliderInput("duration",
                  "Duration of Protection (years):",
                  min = 1,
                  max = 20,
                  value = 5,
                  step = 1),
      
      # Input: Cost per treatment slider
      sliderInput("cost",
                  "Cost per Treatment (K) in USD:",
                  min = 0,
                  max = 10000,
                  value = 500,
                  step = 100,
                  pre = "$"),
      
      # Input: Prevalence slider (New)
      sliderInput("prevalence",
                  "Prevalence of HIV in Population (P):",
                  min = 0,
                  max = 1,
                  value = 0.1,
                  step = 0.01,
                  post = ""),
      
      hr(),
      
      # Display NNT, C, and E
      h4("Calculated Outputs"),
      verbatimTextOutput("nnt"),
      verbatimTextOutput("costAverted"),
      verbatimTextOutput("efficiencyRatio")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 br(),
                 h3("Cost per Infection Averted (C)"),
                 plotOutput("costEfficacyPlot"),
                 br(),
                 plotOutput("costIncidencePlot"),
                 br(),
                 plotOutput("costDurationPlot"),
                 hr(),
                 h3("Number Needed to Treat (NNT)"),
                 plotOutput("nntEfficacyPlot"),
                 br(),
                 plotOutput("nntIncidencePlot"),
                 br(),
                 plotOutput("nntDurationPlot")
        ),
        tabPanel("Efficiency Ratio",
                 br(),
                 h4("Efficiency Ratio (E) between Treatment as Prevention and Primary Prevention"),
                 
                 # Slider for r
                 sliderInput("r",
                             "Fraction of Susceptible Population Targeted (r):",
                             min = 0.01,
                             max = 1,
                             value = 0.5,
                             step = 0.01),
                 
                 # Display E
                 h4("Calculated Efficiency Ratio (E)"),
                 verbatimTextOutput("efficiencyRatioDisplay"),
                 
                 # Plot E vs r
                 plotOutput("efficiencyPlot")
        ),
        tabPanel("Documentation",
                 br(),
                 h4("About this app"),
                 p("This app is based on the publication "'Simple calculations of direct impact for the initial assessment of the value of primary HIV prevention interventions', by
                   Geoff Garnett, Josh Herbeck, and Adam Akullian""),
                 p("This app calculates the Number Needed to Treat (NNT), the Cost per Infection Averted (C), and the Efficiency Ratio (E) between Treatment as Prevention (TasP) and Primary Prevention for HIV interventions."),
                 
                 h5("Inputs:"),
                 tags$ul(
                   tags$li(strong("Efficacy (e):"), " The effectiveness of the HIV prevention intervention as a percentage."),
                   tags$li(strong("HIV Incidence Without Intervention (I):"), " The rate of new HIV infections per 100 person-years in the absence of the intervention."),
                   tags$li(strong("Duration of Protection:"), " The number of years the intervention provides protection against HIV."),
                   tags$li(strong("Cost per Treatment (K):"), " The cost in USD for each treatment or intervention unit."),
                   tags$li(strong("Prevalence of HIV in Population (P):"), " The proportion of the population that is HIV positive, ranging from 0 to 1."),
                   tags$li(strong("Fraction of Susceptible Population Targeted (r):"), " The proportion of the HIV-susceptible population targeted by the intervention, ranging from 0.01 to 1.")
                 ),
                 
                 h5("Outputs:"),
                 tags$ul(
                   tags$li(strong("Number Needed to Treat (NNT):"), " The number of individuals that need to receive the intervention to prevent one HIV infection."),
                   tags$li(strong("Cost per Infection Averted (C):"), " The total cost required to avert one HIV infection."),
                   tags$li(strong("Efficiency Ratio (E):"), " The efficiency ratio between Treatment as Prevention (TasP) and Primary Prevention strategies.")
                 ),
                 
                 h5("Equations:"),
                 p("The calculations for NNT, C, and E are based on the following formulas:"),
                 
                 # Displaying the equations using LaTeX
                 withMathJax(
                   helpText(
                     "\\[ \\text{Number Needed to Treat (NNT)} = \\frac{1}{e \\times I \\times \\text{duration}} \\]"
                   ),
                   helpText(
                     "\\[ \\text{Cost per Infection Averted (C)} = K \\times \\text{NNT} \\]"
                   ),
                   helpText(
                     "\\[ \\text{Efficiency Ratio (E)} = \\frac{P}{r \\times (1 - P)} \\]"
                   )
                 ),
                 
                 h5("Explanation of Equations:"),
                 tags$ul(
                   tags$li(
                     strong("Number Needed to Treat (NNT):"),
                     " This represents the number of individuals who need to undergo the intervention to prevent one HIV infection. It is calculated by taking the reciprocal of the product of efficacy (e), incidence without intervention (I), and the duration of protection."
                   ),
                   tags$li(
                     strong("Cost per Infection Averted (C):"),
                     " This indicates the total cost required to prevent one HIV infection. It is determined by multiplying the cost per treatment (K) by the Number Needed to Treat (NNT)."
                   ),
                   tags$li(
                     strong("Efficiency Ratio (E):"),
                     " The Efficiency Ratio (E) measures the efficiency between Treatment as Prevention (TasP) and Primary Prevention strategies. It is calculated as:",
                     withMathJax(
                       helpText("\\[ E = \\frac{P}{r \\times (1 - P)} \\]")
                     )
                   )
                 ),
                 
                 h5("Detailed Breakdown:"),
                 tags$ol(
                   tags$li(
                     strong("Efficacy (e):"),
                     " Expressed as a decimal, it represents the proportion of effectiveness of the intervention. For example, 50% efficacy is represented as 0.5."
                   ),
                   tags$li(
                     strong("HIV Incidence Without Intervention (I):"),
                     " Also expressed as a decimal per person-year, it reflects the rate at which new HIV infections occur in the absence of the intervention."
                   ),
                   tags$li(
                     strong("Duration of Protection:"),
                     " The number of years the intervention remains effective in preventing HIV infection."
                   ),
                   tags$li(
                     strong("Cost per Treatment (K):"),
                     " The monetary cost associated with administering one unit of the intervention."
                   ),
                   tags$li(
                     strong("Prevalence of HIV in Population (P):"),
                     " The proportion of the population that is HIV positive. It ranges from 0 to 1."
                   ),
                   tags$li(
                     strong("Fraction of Susceptible Population Targeted (r):"),
                     " The proportion of the HIV-susceptible population that is targeted by the intervention. It ranges from 0.01 to 1 to avoid division by zero."
                   )
                 ),
                 
                 h5("Example Calculations:"),
                 p("Suppose an intervention has an efficacy of 60% (0.6), an HIV incidence of 2 per 100 person-years (0.02), provides protection for 5 years, targets 50% of the susceptible population (\\( r = 0.5 \\)), the prevalence of HIV in the population is 10% (\\( P = 0.1 \\)), and the cost per treatment is 500. Then:"),
                 
                 withMathJax(
                   helpText(
                     "\\[ \\text{NNT} = \\frac{1}{0.6 \\times 0.02 \\times 5} = \\frac{1}{0.06} \\approx 16.67 \\]"
                   ),
                   helpText(
                     "\\[ \\text{C} = 500 \\times 16.67 = 8,333.33 \\]"
                   ),
                   helpText(
                     "\\[ \\text{E} = \\frac{0.1}{0.5 \\times (1 - 0.1)} = \\frac{0.1}{0.5 \\times 0.9} = \\frac{0.1}{0.45} \\approx 0.2222 \\]"
                   )
                 ),
                 p("Interpretation:"),
                 tags$ul(
                   tags$li("Approximately **16.67 individuals** need to receive the intervention to prevent **one HIV infection** over **5 years**."),
                   tags$li("The **cost to avert one infection** is **8,333.33**."),
                   tags$li("The **Efficiency Ratio (E)** of **0.22** indicates that the Treatment as Prevention strategy is relatively efficient compared to Primary Prevention when targeting 50% of the susceptible population with a prevalence of 10%.")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expressions to calculate NNT and Cost per Infection Averted
  calculations <- reactive({
    # Convert efficacy percentage to proportion
    e <- input$efficacy / 100
    I <- input$incidence / 100  # Convert per 100 person-years to proportion
    duration <- input$duration
    K <- input$cost
    
    # Calculate NNT
    # NNT = 1 / (e * I * duration)
    if (e * I * duration > 0){
      NNT <- 1 / (e * I * duration)
    } else {
      NNT <- NA
    }
    
    # Calculate Cost per Infection Averted
    # C = K * NNT
    if (!is.na(NNT)){
      C <- K * NNT
    } else {
      C <- NA
    }
    
    list(NNT = NNT, C = C)
  })
  
  # Output NNT
  output$nnt <- renderText({
    calc <- calculations()
    if (is.na(calc$NNT)){
      "Number Needed to Treat (NNT): Undefined (Check input values)"
    } else {
      paste("Number Needed to Treat (NNT):", round(calc$NNT, 2))
    }
  })
  
  # Output Cost per Infection Averted
  output$costAverted <- renderText({
    calc <- calculations()
    if (is.na(calc$C)){
      "Cost per Infection Averted (C): Undefined (Check input values)"
    } else {
      paste("Cost per Infection Averted (C): $", round(calc$C, 2), sep = "")
    }
  })
  
  # Plot Cost per Infection Averted vs Efficacy
  output$costEfficacyPlot <- renderPlot({
    e_seq <- seq(1, 100, by = 1)
    I <- input$incidence / 100
    duration <- input$duration
    K <- input$cost
    
    C_values <- ifelse(e_seq > 0,
                       K / ( (e_seq / 100) * I * duration),
                       NA)
    
    data <- data.frame(Efficacy = e_seq, CostAverted = C_values)
    
    ggplot(data, aes(x = Efficacy, y = CostAverted)) +
      geom_line(color = "blue") +
      labs(title = "Cost per Infection Averted vs Efficacy",
           x = "Efficacy (%)",
           y = "Cost per Infection Averted (USD)") +
      theme_minimal()
  })
  
  # Plot Cost per Infection Averted vs Incidence (Updated to 0-4)
  output$costIncidencePlot <- renderPlot({
    I_seq <- seq(0.1, 4, by = 0.1)  # Updated range
    e <- input$efficacy / 100
    duration <- input$duration
    K <- input$cost
    
    C_values <- ifelse(I_seq > 0,
                       K / ( e * (I_seq / 100) * duration),
                       NA)
    
    data <- data.frame(Incidence = I_seq, CostAverted = C_values)
    
    ggplot(data, aes(x = Incidence, y = CostAverted)) +
      geom_line(color = "darkgreen") +
      labs(title = "Cost per Infection Averted vs Incidence",
           x = "HIV Incidence Without Intervention (per 100 person-years)",
           y = "Cost per Infection Averted (USD)") +
      theme_minimal()
  })
  
  # Plot Cost per Infection Averted vs Duration of Protection
  output$costDurationPlot <- renderPlot({
    duration_seq <- seq(1, 20, by = 1)
    e <- input$efficacy / 100
    I <- input$incidence / 100
    K <- input$cost
    
    C_values <- ifelse(duration_seq > 0,
                       K / ( e * I * duration_seq),
                       NA)
    
    data <- data.frame(Duration = duration_seq, CostAverted = C_values)
    
    ggplot(data, aes(x = Duration, y = CostAverted)) +
      geom_line(color = "purple") +
      labs(title = "Cost per Infection Averted vs Duration of Protection",
           x = "Duration of Protection (years)",
           y = "Cost per Infection Averted (USD)") +
      theme_minimal()
  })
  
  # Plot Number Needed to Treat vs Efficacy
  output$nntEfficacyPlot <- renderPlot({
    e_seq <- seq(1, 100, by = 1)
    I <- input$incidence / 100
    duration <- input$duration
    
    NNT_values <- ifelse(e_seq > 0,
                         1 / ( (e_seq / 100) * I * duration ),
                         NA)
    
    data <- data.frame(Efficacy = e_seq, NNT = NNT_values)
    
    ggplot(data, aes(x = Efficacy, y = NNT)) +
      geom_line(color = "red") +
      labs(title = "Number Needed to Treat vs Efficacy",
           x = "Efficacy (%)",
           y = "Number Needed to Treat (NNT)") +
      theme_minimal()
  })
  
  # Plot Number Needed to Treat vs Incidence (Updated to 0-4)
  output$nntIncidencePlot <- renderPlot({
    I_seq <- seq(0.1, 4, by = 0.1)  # Updated range
    e <- input$efficacy / 100
    duration <- input$duration
    
    NNT_values <- ifelse(I_seq > 0,
                         1 / ( e * (I_seq / 100) * duration ),
                         NA)
    
    data <- data.frame(Incidence = I_seq, NNT = NNT_values)
    
    ggplot(data, aes(x = Incidence, y = NNT)) +
      geom_line(color = "orange") +
      labs(title = "Number Needed to Treat vs Incidence",
           x = "HIV Incidence Without Intervention (per 100 person-years)",
           y = "Number Needed to Treat (NNT)") +
      theme_minimal()
  })
  
  # Plot Number Needed to Treat vs Duration of Protection
  output$nntDurationPlot <- renderPlot({
    duration_seq <- seq(1, 20, by = 1)
    e <- input$efficacy / 100
    I <- input$incidence / 100
    
    NNT_values <- ifelse(duration_seq > 0,
                         1 / ( e * I * duration_seq ),
                         NA)
    
    data <- data.frame(Duration = duration_seq, NNT = NNT_values)
    
    ggplot(data, aes(x = Duration, y = NNT)) +
      geom_line(color = "brown") +
      labs(title = "Number Needed to Treat vs Duration of Protection",
           x = "Duration of Protection (years)",
           y = "Number Needed to Treat (NNT)") +
      theme_minimal()
  })
  
  # Reactive expression to calculate Efficiency Ratio (E)
  efficiency_calculation <- reactive({
    r <- input$r  # Fraction targeted
    P <- input$prevalence  # Prevalence
    # To avoid division by zero if r*(1-P) is zero
    if (r * (1 - P) > 0) {
      E <- P / (r * (1 - P))
    } else {
      E <- NA
    }
    E
  })
  
  # Output Efficiency Ratio in Sidebar
  output$efficiencyRatio <- renderText({
    E <- efficiency_calculation()
    if (is.na(E)) {
      "Efficiency Ratio (E): Undefined (Check input values)"
    } else {
      paste("Efficiency Ratio (E):", round(E, 4))
    }
  })
  
  # Output Efficiency Ratio Display in Efficiency Ratio Tab
  output$efficiencyRatioDisplay <- renderText({
    E <- efficiency_calculation()
    if (is.na(E)) {
      "Efficiency Ratio (E): Undefined (Check input values)"
    } else {
      paste("Efficiency Ratio (E):", round(E, 4))
    }
  })
  
  # Plot Efficiency Ratio vs r
  output$efficiencyPlot <- renderPlot({
    r_seq <- seq(0.01, 1, by = 0.01)  # Avoid r = 0 to prevent division by zero
    P <- input$prevalence  # Prevalence
    E_values <- P / (r_seq * (1 - P))
    
    data <- data.frame(r = r_seq, EfficiencyRatio = E_values)
    
    ggplot(data, aes(x = r, y = EfficiencyRatio)) +
      geom_line(color = "darkblue") +
      labs(title = "Efficiency Ratio (E) vs Fraction Targeted (r)",
           x = "Fraction of Susceptible Population Targeted (r)",
           y = "Efficiency Ratio (E)") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
