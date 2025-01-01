# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Temperature Trend Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      # Input for the average temperature
      numericInput("avgTemp", "Average Temperature (°C):", value = 20, min = -30, max = 50),
      # Input for the variation range (standard deviation)
      numericInput("variation", "Variation Range (°C):", value = 5, min = 1, max = 20),
      # Input for the number of days to simulate
      numericInput("days", "Number of Days:", value = 30, min = 1, max = 365),
      # Button to generate the temperature data
      actionButton("generateButton", "Generate Temperature Trend"),
      # Button to reset the data
      actionButton("resetButton", "Reset Data"),
      br(),
      br(),
      # Option to save the data as a CSV
      downloadButton("downloadData", "Download Temperature Data")
    ),
    
    mainPanel(
      # Output: Time-series plot for the temperature trend
      plotOutput("tempPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to generate the temperature data
  temp_data <- reactiveVal(NULL)
  
  # Generate the temperature data when the button is pressed
  observeEvent(input$generateButton, {
    avgTemp <- input$avgTemp
    variation <- input$variation
    days <- input$days
    
    # Simulate daily temperatures using rnorm()
    temperatures <- rnorm(days, mean = avgTemp, sd = variation)
    
    # Create a data frame with the day and temperature values
    temp_df <- data.frame(Day = 1:days, Temperature = temperatures)
    
    # Store the generated data
    temp_data(temp_df)
  })
  
  # Reset the data when the reset button is pressed
  observeEvent(input$resetButton, {
    temp_data(NULL)
  })
  
  # Render the time-series plot
  output$tempPlot <- renderPlot({
    # Check if there is data to plot
    if (!is.null(temp_data())) {
      ggplot(temp_data(), aes(x = Day, y = Temperature)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        labs(title = "Simulated Daily Temperature Trend", x = "Day", y = "Temperature (°C)") +
        theme_minimal()
    }
  })
  
  # Download the generated data as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("temperature_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(temp_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)