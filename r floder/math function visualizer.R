# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Math Function Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("math_function", "Enter Function (e.g., sin(x), x^2):", value = "sin(x)"),
      sliderInput("range", "Range of x:", min = -10, max = 10, value = c(-10, 10)),
      helpText("Supported functions: sin(x), cos(x), tan(x), exp(x), log(x), x^2, etc.")
    ),
    
    mainPanel(
      plotOutput("mathPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to evaluate mathematical expressions
  eval_function <- reactive({
    # Parse the input function safely
    expr <- input$math_function  # Renamed to math_function to avoid conflict
    
    # Ensure the input is safe and can be evaluated
    expr_safe <- gsub("x", "x_val", expr)  # Replace 'x' with 'x_val' to avoid conflicts
    
    # Generate x values based on the range slider
    x_vals <- seq(input$range[1], input$range[2], length.out = 1000)
    
    # Try to evaluate the function for each x value
    y_vals <- sapply(x_vals, function(x_val) {
      tryCatch(
        eval(parse(text = expr_safe)),  # Evaluate the expression
        error = function(e) NA  # Return NA if there is an error (e.g., invalid expression)
      )
    })
    
    # Return data frame for plotting
    data.frame(x = x_vals, y = y_vals)
  })
  
  # Render plot based on the selected function
  output$mathPlot <- renderPlot({
    data <- eval_function()
    
    ggplot(data, aes(x = x, y = y)) +
      geom_line(color = "blue") +
      labs(title = paste("Plot of", input$math_function),
           x = "x", y = "f(x)") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
