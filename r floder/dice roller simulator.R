# Load necessary library
library(shiny)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Dice Roller Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Number of dice to roll
      numericInput("numDice", "Number of Dice:", value = 1, min = 1, max = 10),
      # Button to roll the dice
      actionButton("rollButton", "Roll Dice"),
      br(),
      br(),
      # Display the result as text
      textOutput("resultText"),
      br(),
      # Option to select if we want a bar plot or not
      checkboxInput("showPlot", "Show Frequency Plot", value = FALSE)
    ),
    
    mainPanel(
      # Output: Bar plot for frequency of rolls (optional)
      plotOutput("dicePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to roll the dice when the button is pressed
  rolled_dice <- eventReactive(input$rollButton, {
    # Simulate rolling 'n' dice (where n is input$numDice)
    sample(1:6, size = input$numDice, replace = TRUE)
  })
  
  # Render the result as text
  output$resultText <- renderText({
    dice_rolls <- rolled_dice()
    paste("You rolled: ", paste(dice_rolls, collapse = ", "))
  })
  
  # Render the bar plot if the checkbox is checked
  output$dicePlot <- renderPlot({
    if (input$showPlot) {
      dice_rolls <- rolled_dice()
      # Create a data frame for plotting
      roll_data <- as.data.frame(table(dice_rolls))
      names(roll_data) <- c("Roll", "Frequency")
      
      ggplot(roll_data, aes(x = factor(Roll), y = Frequency)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Frequency of Dice Rolls", x = "Dice Face", y = "Frequency") +
        theme_minimal()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)