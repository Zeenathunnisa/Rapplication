# Load the Shiny package
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("BMI Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "weight",
        label = "Weight (kg):",
        value = 70,
        min = 1,
        step = 0.1
      ),
      numericInput(
        inputId = "height",
        label = "Height (m):",
        value = 1.75,
        min = 0.5,
        step = 0.01
      ),
      actionButton(
        inputId = "calculate",
        label = "Calculate BMI"
      )
    ),
    
    mainPanel(
      h4("Your BMI:"),
      textOutput("bmiValue"),
      h4("Category:"),
      textOutput("bmiCategory")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive expression to calculate BMI
  bmi <- eventReactive(input$calculate, {
    weight <- input$weight
    height <- input$height
    if (height > 0) {
      bmi <- weight / (height^2)
      return(round(bmi, 2))
    } else {
      return(NA)
    }
  })
  
  # Determine BMI category
  category <- reactive({
    if (is.na(bmi())) {
      return("Invalid height or weight")
    } else if (bmi() < 18.5) {
      return("Underweight")
    } else if (bmi() >= 18.5 && bmi() < 24.9) {
      return("Normal weight")
    } else if (bmi() >= 25 && bmi() < 29.9) {
      return("Overweight")
    } else {
      return("Obesity")
    }
  })
  
  # Render BMI value
  output$bmiValue <- renderText({
    if (!is.na(bmi())) {
      paste(bmi())
    } else {
      "Invalid input"
    }
  })
  
  # Render BMI category
  output$bmiCategory <- renderText({
    category()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

