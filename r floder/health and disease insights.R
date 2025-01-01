library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Updated simulated healthcare and disease dataset with more diseases (15 entries)
disease_data <- data.frame(
  State = c("Maharashtra", "Uttar Pradesh", "Kerala", "Tamil Nadu", "Rajasthan", 
            "Gujarat", "West Bengal", "Karnataka", "Punjab", "Delhi",
            "Bihar", "Andhra Pradesh", "Madhya Pradesh", "Odisha", "Haryana"),
  Population = c(112374333, 199812341, 33406061, 72147030, 68548437, 
                 60439692, 91276115, 61095297, 27743338, 16787941,
                 104099452, 49386799, 72830000, 41594741, 25353081),
  Hospitals = c(1200, 1800, 900, 1500, 1100, 1000, 1300, 1400, 800, 700,
                950, 1200, 1100, 800, 600),
  Disease = c("Diabetes", "Hypertension", "Cancer", "Respiratory", "Diabetes", 
              "Hypertension", "Cancer", "Respiratory", "Diabetes", "Hypertension",
              "Stroke", "Malaria", "Tuberculosis", "Asthma", "Cancer"),
  Cases = c(250000, 400000, 150000, 200000, 220000, 
            300000, 180000, 170000, 120000, 90000,
            50000, 150000, 80000, 120000, 160000)
)

# UI Definition
ui <- fluidPage(
  titlePanel("Healthcare and Disease Insights"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select Metric:",
                  choices = c("Population", "Hospitals", "Cases"),
                  selected = "Cases"),
      selectInput("disease", "Select Disease:",
                  choices = unique(disease_data$Disease),
                  selected = "Diabetes"),
      sliderInput("transparency", "Adjust Transparency:",
                  min = 0.1, max = 1.0, value = 0.7, step = 0.1)
    ),
    mainPanel(
      plotOutput("stateMap"),
      tableOutput("summaryTable")
    )
  )
)

# Server Logic
server <- function(input, output) {
  filteredData <- reactive({
    disease_data %>% filter(Disease == input$disease)
  })
  
  output$stateMap <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = reorder(State, -.data[[input$metric]]), y = .data[[input$metric]], fill = .data[[input$metric]])) +
      geom_bar(stat = "identity", alpha = input$transparency) +
      scale_fill_viridis(option = "C", name = input$metric) +
      labs(title = paste(input$metric, "Distribution for", input$disease),
           x = "State", y = input$metric) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summaryTable <- renderTable({
    filteredData() %>% 
      arrange(desc(.data[[input$metric]])) %>%
      select(State, Population, Hospitals, Cases)
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
