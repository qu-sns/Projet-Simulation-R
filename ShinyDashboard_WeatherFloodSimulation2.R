# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Probability Simulation App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Evolution Plot", tabName = "evolution", icon = icon("area-chart")),
      menuItem("Histogram", tabName = "histogram", icon = icon("bar-chart")),
      menuItem("Hmax Distribution", tabName = "hmax", icon = icon("line-chart")),
      menuItem("Inundation Probability", tabName = "inundation", icon = icon("bar-chart")),
      menuItem("Scatter Plot", tabName = "scatter", icon = icon("dot-circle")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
      menuItem("Summary Table", tabName = "summary", icon = icon("table")),
      menuItem("Parameter Sensitivity", tabName = "sensitivity", icon = icon("sliders"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard tab (placeholder)
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "WARNING - DISCLAIMER !", 
                    div(
                      p("L'existence de cette plateforme en particulier relève uniquement de mon intérêt personnel de vouloir apprendre en profondeur les librairies 'Shiny' et 'ShinyDashboard' de R, ainsi que de créer une application permettant à une tierce personne de pouvoir naviguer simplement à travers les différents résultats de simulations.
La réalisation de cette application n'a absolument pas été demandé ni requis dans le cadre du projet scolaire, dont l'unique but était de pouvoir réussir les simulations mais pas de créer une application."),
                      p("TOUS DROITS RÉSERVÉS"),
                      p("Dinesh BALASOUPRAMANIANE"),
                      p("ISFA 2023/2024"),
                      h3("Bienvenue !"),
                      p("Bienvenue sur l'application RShiny (Dashboard) pour la simulation de processus de Poisson sur le risque d'inondation par la pluie."),
                      p("Cette plateforme a pour but d'afficher de manière interactive et dynamique des visualisations graphiques de données relativement au projet de simulation du cours de 'Techniques de Simulation' à l'ISFA."),
                      p("Vous pouvez naviguer à travers la barre d'onglet sur la gauche pour visualiser les différent graphiques et analyses."),
                      p("Dinesh.B - Quentin.S - Evrard.D"),
                      img(src = ".png", height = 300, width = 300)
                      
                      
                    )
                )
              )
      ),
      
      # Other tabs
      tabItem(tabName = "evolution",
              fluidRow(
                sliderInput("param_value", "Parameter Value", value = 1, min = 0.1, max = 2),
                sliderInput("h0", "Threshold (h0)", value = 10, min = 5, max = 15),
                numericInput("n_simul", "Number of Simulations", value = 100, min = 10, max = 1000),
                plotOutput("selected_plot_evolution")
              )
      ),
      
      tabItem(tabName = "histogram",
              fluidRow(
                plotOutput("selected_plot_histogram")
              )
      ),
      
      tabItem(tabName = "hmax",
              fluidRow(
                plotOutput("selected_plot_hmax")
              )
      ),
      
      tabItem(tabName = "inundation",
              fluidRow(
                plotOutput("selected_plot_inundation")
              )
      ),
      
      tabItem(tabName = "scatter",
              fluidRow(
                plotOutput("selected_plot_scatter")
              )
      ),
      
      tabItem(tabName = "timeseries",
              fluidRow(
                plotOutput("selected_plot_timeseries")
              )
      ),
      
      tabItem(tabName = "summary",
              fluidRow(
                tableOutput("selected_table_summary")
              )
      ),
      
      tabItem(tabName = "sensitivity",
              fluidRow(
                plotOutput("selected_plot_sensitivity")
              )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  # Placeholder for simulation results
  simulated_data <- reactive({
    # Call your simulation function here with updated parameters
    data.frame(prob = rnorm(input$n_simul, mean = 0.5, sd = 0.2),
               hmax = runif(input$n_simul, min = 5, max = 15))
  })
  
  # Dynamic plots based on user selection
  output$selected_plot_evolution <- renderPlot({
    ggplot(simulated_data(), aes(x = 1:input$n_simul, y = prob)) +
      geom_line() +
      labs(title = "Evolution of Probability", x = "Simulation Number", y = "Probability")
  })
  
  output$selected_plot_histogram <- renderPlot({
    ggplot(simulated_data(), aes(x = prob)) +
      geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = "Histogram of Simulated Probabilities", x = "Probability", y = "Frequency")
  })
  
  output$selected_plot_hmax <- renderPlot({
    ggplot(simulated_data(), aes(x = hmax)) +
      geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
      labs(title = "Distribution of Maximum Water Height (Hmax)", x = "Hmax", y = "Frequency")
  })
  
  output$selected_plot_inundation <- renderPlot({
    h0_values <- seq(5, 15, by = 0.1)
    inundation_prob <- sapply(h0_values, function(h) {
      mean(simulated_data()$hmax > h)
    })
    
    ggplot(data.frame(h0 = h0_values, Inundation_Probability = inundation_prob), aes(x = h0, y = Inundation_Probability)) +
      geom_line() +
      labs(title = "Relationship between Threshold (h0) and Inundation Probability", x = "Threshold (h0)", y = "Inundation Probability")
  })
  
  output$selected_plot_scatter <- renderPlot({
    ggplot(simulated_data(), aes(x = prob, y = hmax)) +
      geom_point(alpha = 0.5) +
      labs(title = "Scatter Plot of Probability and Hmax", x = "Probability", y = "Hmax")
  })
  
  output$selected_plot_timeseries <- renderPlot({
    time_values <- seq(0, 1, length.out = input$n_simul)
    ggplot(simulated_data(), aes(x = time_values, y = hmax)) +
      geom_line() +
      labs(title = "Time Series Plot of Hmax", x = "Time", y = "Hmax")
  })
  
  output$selected_table_summary <- renderTable({
    summary_stats <- simulated_data() %>%
      summarise(Mean_Probability = mean(prob),
                Mean_Hmax = mean(hmax),
                SD_Hmax = sd(hmax),
                Inundation_Prob = mean(hmax > input$h0))
    return(summary_stats)
  })
  
  output$selected_plot_sensitivity <- renderPlot({
    param_values <- seq(input$param_value - 0.5, input$param_value + 0.5, length.out = 100)
    prob_values <- sapply(param_values, function(val) {
      mean(simulated_data()$hmax > val)
    })
    
    sensitivity_data <- data.frame(Parameter_Value = param_values, Inundation_Probability = prob_values)
    
    ggplot(sensitivity_data, aes(x = Parameter_Value, y = Inundation_Probability)) +
      geom_line() +
      labs(title = "Parameter Sensitivity Analysis",
           x = paste("Parameter Value (", "Parameter", ")", sep = ""),
           y = "Inundation Probability") +
      geom_point(aes(x = input$param_value, y = tail(prob_values, 1)), color = "red", size = 3) +
      geom_text(aes(x = input$param_value, y = tail(prob_values, 1)),
                label = scales::number_format(accuracy = 0.01)(tail(prob_values, 1)),
                vjust = -0.5, hjust = 1, color = "red")
  })
}

# Run the app
shinyApp(ui, server)