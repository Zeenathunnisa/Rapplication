# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(plotly)

# Define list of states and crime types
states <- c("Delhi", "Mumbai", "Kolkata", "Chennai", "Bangalore", "Hyderabad", "Pune", "Lucknow", "Jaipur", "Ahmedabad")
crime_types <- c("Theft", "Assault", "Robbery", "Murder", "Fraud", "Sexual Assault", "Domestic Violence", "Drug Offenses", "Corruption", "Riots")

# Simulate crime data for years 2000 to 2024 for all states and crime types
set.seed(123)
crime_data <- expand.grid(
  Year = 2000:2024,
  State = states,
  Crime_Type = crime_types
)

# Adding crime counts (random data for simulation, will generate zero if needed later)
crime_data$Crime_Count <- sample(100:1000, nrow(crime_data), replace = TRUE)

# Ensure that all combinations are present
crime_data_complete <- crime_data %>%
  complete(Year, State, Crime_Type, fill = list(Crime_Count = 0))

# Define UI for the application
ui <- fluidPage(
  
  # Application title
  titlePanel("Interactive Crime Data Exploration Across India"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      
      # Select input for State
      selectInput("state", "Select State:", choices = unique(crime_data_complete$State), selected = "Delhi"),
      
      # Select input for Year range (2000 to 2024)
      sliderInput("year", "Select Year Range:", min = 2000, max = 2024, value = c(2000, 2024), step = 1),
      
      # Select input for Crime Type
      selectInput("crime_type", "Select Crime Type:", choices = unique(crime_data_complete$Crime_Type), selected = "Theft"),
      
      # Select input for Graph Type
      selectInput("graph_type", "Select Graph Type:", choices = c("Bar Chart", "Line Chart", "Pie Chart"), selected = "Bar Chart")
    ),
    
    # Main panel for displaying the plot and the data table
    mainPanel(
      plotlyOutput("crimePlot"),   # Crime graph output
      DTOutput("dataTable")        # Data table output
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filter crime data based on inputs
  filtered_data <- reactive({
    crime_data_complete %>%
      filter(State == input$state,
             Year >= input$year[1], Year <= input$year[2],
             Crime_Type == input$crime_type)
  })
  
  # Render the crime plot based on selected graph type
  output$crimePlot <- renderPlotly({
    data <- filtered_data()
    
    if (input$graph_type == "Bar Chart") {
      p <- ggplot(data, aes(x = as.factor(Year), y = Crime_Count, fill = Crime_Type)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Year", y = "Crime Count", title = paste(input$crime_type, "in", input$state)) +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$graph_type == "Line Chart") {
      p <- ggplot(data, aes(x = Year, y = Crime_Count, group = Crime_Type, color = Crime_Type)) +
        geom_line() +
        geom_point() +
        labs(x = "Year", y = "Crime Count", title = paste(input$crime_type, "Trends in", input$state)) +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$graph_type == "Pie Chart") {
      pie_data <- data %>%
        group_by(Crime_Type) %>%
        summarise(total_crimes = sum(Crime_Count)) %>%
        ungroup()
      
      p <- plot_ly(pie_data, labels = ~Crime_Type, values = ~total_crimes, type = 'pie',
                   title = paste("Distribution of", input$crime_type, "in", input$state))
      p
    }
  })
  
  # Render the crime data table
  output$dataTable <- renderDT({
    filtered_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
