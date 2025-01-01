# Load necessary libraries
library(shiny)

# Define UI for the Guess the Number game
ui <- fluidPage(
  titlePanel("Guess the Number Game"),
  
  sidebarLayout(
    sidebarPanel(
      p("Try to guess the number between 1 and 100!"),
      textInput("guess", "Enter your guess:", value = ""),
      actionButton("submit_btn", "Submit Guess"),
      h3("Feedback:"),
      textOutput("feedback"),
      h3("Score:"),
      textOutput("score"),
      actionButton("reset_btn", "Start New Game")
    ),
    
    mainPanel(
      h3("Instructions"),
      p("The game generates a random number between 1 and 100. Try to guess it by entering your number in the input box and clicking 'Submit Guess'. After each guess, you will be told if your guess is too high, too low, or correct. Good luck!")
    )
  )
)

# Define server logic for the Guess the Number game
server <- function(input, output, session) {
  
  # Initialize variables for the random number and score
  secret_number <- reactiveVal(sample(1:100, 1))  # Random number between 1 and 100
  attempts <- reactiveVal(0)  # Number of attempts
  game_over <- reactiveVal(FALSE)  # Flag to check if the game is over
  
  # Output the feedback and score
  output$feedback <- renderText({
    if (game_over()) {
      return("Game Over! You guessed the number!")
    } else if (input$guess == "") {
      return("Enter your guess and click 'Submit Guess'.")
    } else {
      guess <- as.numeric(input$guess)
      if (is.na(guess)) {
        return("Please enter a valid number.")
      } else if (guess < secret_number()) {
        return("Too low! Try again.")
      } else if (guess > secret_number()) {
        return("Too high! Try again.")
      } else {
        return("Correct! You guessed the number!")
      }
    }
  })
  
  output$score <- renderText({
    paste("Attempts: ", attempts())
  })
  
  # Action when the submit button is clicked
  observeEvent(input$submit_btn, {
    if (game_over()) {
      return()  # Do nothing if the game is over
    }
    
    guess <- as.numeric(input$guess)
    
    if (!is.na(guess)) {
      attempts(attempts() + 1)  # Increment attempts
      
      if (guess == secret_number()) {
        game_over(TRUE)  # End game if the correct number is guessed
      }
    }
  })
  
  # Action when the reset button is clicked (new game)
  observeEvent(input$reset_btn, {
    secret_number(sample(1:100, 1))  # Generate a new random number
    attempts(0)  # Reset attempts
    game_over(FALSE)  # Reset game over flag
    updateTextInput(session, "guess", value = "")  # Clear the input box
  })
}

# Run the application
shinyApp(ui = ui, server = server)
