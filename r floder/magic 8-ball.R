# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define a broader set of possible responses for any type of question
answers <- c(
  "Yes, definitely.",
  "No, absolutely not.",
  "Ask again later.",
  "Maybe, but not likely.",
  "It’s a secret!",
  "The stars are not aligned.",
  "Don’t count on it.",
  "Yes, but only if you work for it.",
  "Absolutely, but you must believe in yourself.",
  "The future is unclear, but the path is yours to shape.",
  "It is certain!",
  "I have no clue, but good luck!",
  "In your dreams!",
  "I would not count on it.",
  "Signs point to yes.",
  "I cannot predict that right now.",
  "Perhaps, but you need to focus more.",
  "It is up to you."
)

# Define the specific questions and answers
specific_answers <- list(
  "what's your name" = "I am the Magic 8-Ball! I don't have a specific name, but you can call me 8-Ball.",
  "how are you" = "I’m just a magical ball, but I’m doing well, thank you for asking!",
  "who are you" = "I am the Magic 8-Ball, a mystical oracle of answers!",
  "tell me a joke" = "Why don’t skeletons fight each other? They don’t have the guts!",
  "what is the meaning of life" = "The meaning of life is to find meaning. Seek and you shall find your purpose."
)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Magic 8-Ball: Ask Any Question!"),
  
  # Sidebar layout with a question input and action button
  sidebarLayout(
    sidebarPanel(
      textInput("question", "Ask a Question:", ""),
      actionButton("askButton", "Ask the Magic 8-Ball", class = "btn-primary"),
      actionButton("resetButton", "Reset History", class = "btn-danger"),
      hr(),
      textOutput("responseText"),
      hr(),
      helpText("Ask anything, and the Magic 8-Ball will answer you!")
    ),
    
    # Main panel for displaying the response and history plot
    mainPanel(
      h3("Magic 8-Ball Response History"),
      plotOutput("responsePlot"),
      hr(),
      p("Enjoy the magical wisdom of the 8-Ball!")
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  
  # Reactive value to store responses
  responses <- reactiveVal(data.frame(Response = character(), Count = integer(), stringsAsFactors = FALSE))
  
  # When the user presses the ask button, generate a random response
  observeEvent(input$askButton, {
    # Check if the question is not empty
    if (input$question != "") {
      
      # Check if the question matches any specific question
      question_lower <- tolower(input$question) # Convert question to lowercase for case-insensitive comparison
      
      if (question_lower %in% names(specific_answers)) {
        # If the question matches a specific predefined question, use the specific answer
        response <- specific_answers[[question_lower]]
      } else {
        # Otherwise, provide a random response from the general answers list
        response <- sample(answers, 1)
      }
      
      # Update response history with the new response
      current_responses <- responses()
      if (nrow(current_responses) == 0 || !(response %in% current_responses$Response)) {
        new_data <- data.frame(Response = response, Count = 1, stringsAsFactors = FALSE)
        updated_responses <- rbind(current_responses, new_data)
      } else {
        updated_responses <- current_responses
        updated_responses$Count[updated_responses$Response == response] <- updated_responses$Count[updated_responses$Response == response] + 1
      }
      
      # Update the reactive value with new response history
      responses(updated_responses)
      
      # Display the response text
      output$responseText <- renderText({
        paste("Magic 8-Ball says:", response)
      })
    } else {
      # Handle case when the user doesn't enter any question
      output$responseText <- renderText({
        "Please ask a question to the Magic 8-Ball!"
      })
    }
  })
  
  # Render the plot showing the frequency of responses
  output$responsePlot <- renderPlot({
    # Get the response history
    response_history <- responses()
    
    # If there are no responses yet, return an empty plot
    if (nrow(response_history) == 0) {
      return(NULL)
    }
    
    # Create a bar plot showing the frequency of responses
    ggplot(response_history, aes(x = Response, y = Count, fill = Response)) +
      geom_bar(stat = "identity") +
      labs(x = "Magic 8-Ball Response", y = "Count", title = "Frequency of Magic 8-Ball Responses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Reset the response history when the reset button is clicked
  observeEvent(input$resetButton, {
    responses(data.frame(Response = character(), Count = integer(), stringsAsFactors = FALSE))
    output$responseText <- renderText("History has been reset. Ask a new question!")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
