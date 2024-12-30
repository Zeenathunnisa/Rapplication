library(shiny)

# Define the Tic-Tac-Toe UI
ui <- fluidPage(
  titlePanel("Tic-Tac-Toe Game"),
  
  # Game instructions and reset button
  fluidRow(
    column(12, h3("Player 1: X | Player 2: O")),
    column(12, actionButton("reset", "Reset Game", class = "btn-danger"))
  ),
  
  # Game grid with buttons for each cell
  fluidRow(
    column(4, actionButton("btn1", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn2", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn3", "", width = "100px", height = "100px", style = "font-size: 24px"))
  ),
  fluidRow(
    column(4, actionButton("btn4", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn5", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn6", "", width = "100px", height = "100px", style = "font-size: 24px"))
  ),
  fluidRow(
    column(4, actionButton("btn7", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn8", "", width = "100px", height = "100px", style = "font-size: 24px")),
    column(4, actionButton("btn9", "", width = "100px", height = "100px", style = "font-size: 24px"))
  ),
  
  # Text output to show game status
  fluidRow(
    column(12, h4(textOutput("game_status")))
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Initialize a reactive variable to store the game board
  game_board <- reactiveVal(rep("", 9))  # A vector with 9 empty strings representing the board
  current_player <- reactiveVal("X")  # Start with Player 1 (X)
  
  # Check if there's a winner
  check_winner <- function(board) {
    # Winning positions on the board (rows, columns, diagonals)
    winning_combinations <- list(
      c(1, 2, 3), c(4, 5, 6), c(7, 8, 9),  # Rows
      c(1, 4, 7), c(2, 5, 8), c(3, 6, 9),  # Columns
      c(1, 5, 9), c(3, 5, 7)               # Diagonals
    )
    
    for (comb in winning_combinations) {
      if (board[comb[1]] == board[comb[2]] && board[comb[2]] == board[comb[3]] && board[comb[1]] != "") {
        return(board[comb[1]])  # Return the winner (either "X" or "O")
      }
    }
    return(NULL)  # No winner yet
  }
  
  # Update the game board and check the game status after each move
  observe({
    # Update button text based on the board state
    for (i in 1:9) {
      updateActionButton(session, paste0("btn", i), label = game_board()[i])
    }
  })
  
  # Handle button presses
  for (i in 1:9) {
    local({
      j <- i
      observeEvent(input[[paste0("btn", j)]], {
        # Get the current board state
        board <- game_board()
        
        # If the cell is empty, place the current player's mark
        if (board[j] == "") {
          board[j] <- current_player()
          game_board(board)  # Update the board state
          
          # Check if there is a winner after the move
          winner <- check_winner(board)
          if (!is.null(winner)) {
            output$game_status <- renderText(paste("Player", winner, "wins!"))
            return()  # Stop the game after a winner is found
          }
          
          # Change the current player
          current_player(ifelse(current_player() == "X", "O", "X"))
          
          output$game_status <- renderText(paste("Player", current_player(), "'s turn"))
        }
      })
    })
  }
  
  # Reset the game
  observeEvent(input$reset, {
    game_board(rep("", 9))  # Reset the board to empty
    current_player("X")  # Start with Player 1 (X)
    output$game_status <- renderText("Player X's turn")
  })
  
  # Initial game status
  output$game_status <- renderText("Player X's turn")
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
