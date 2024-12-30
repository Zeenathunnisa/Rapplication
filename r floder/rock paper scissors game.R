# Load required libraries
library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Rock, Paper, Scissors Game"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Choose your move:"),
      radioButtons(
        inputId = "user_choice",
        label = NULL,
        choices = c("Rock", "Paper", "Scissors"),
        selected = "Rock"
      ),
      actionButton("play", "Play Game"),
      br(),
      h4("Game Result:"),
      textOutput("result"),
      h4("Computer's Move:"),
      textOutput("computer_move"),
      h4("Your Move:"),
      textOutput("user_move"),
      br(),
      h4("Score:"),
      textOutput("score")
    ),
    
    mainPanel(
      h4("Instructions:"),
      p("1. Choose Rock, Paper, or Scissors."),
      p("2. Press 'Play Game' to see the result."),
      p("3. The computer will randomly pick a move, and the game will tell you if you won, lost, or tied.")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Initialize score
  score <- reactiveVal(0)
  
  # Generate computer's choice
  computer_choice <- function() {
    sample(c("Rock", "Paper", "Scissors"), 1)
  }
  
  # Determine the result of the game
  game_result <- reactive({
    user_choice <- input$user_choice
    comp_choice <- computer_choice()
    
    # Logic to determine the winner
    if (user_choice == comp_choice) {
      result <- "It's a tie!"
    } else if ((user_choice == "Rock" && comp_choice == "Scissors") ||
               (user_choice == "Scissors" && comp_choice == "Paper") ||
               (user_choice == "Paper" && comp_choice == "Rock")) {
      result <- "You win!"
      score(score() + 1)  # Increment score if the user wins
    } else {
      result <- "You lose!"
      score(score() - 1)  # Decrease score if the user loses
    }
    
    # Return the result and the choices
    list(result = result, comp_choice = comp_choice)
  })
  
  # Reactively update the result, computer's choice, and score when the user clicks "Play Game"
  observeEvent(input$play, {
    game_outcome <- game_result()
    
    output$result <- renderText({
      game_outcome$result
    })
    
    output$computer_move <- renderText({
      paste("Computer chose:", game_outcome$comp_choice)
    })
    
    output$user_move <- renderText({
      paste("You chose:", input$user_choice)
    })
    
    output$score <- renderText({
      paste("Score:", score())
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
