# Load required libraries
library(shiny)
library(ggplot2)

# Function to calculate password strength
calculate_strength <- function(password) {
  # Initialize criteria counters
  strength_score <- 0
  criteria <- c("Length", "Numbers", "Uppercase", "Special Characters")
  values <- c(0, 0, 0, 0)
  
  # Length criteria (score +1 for passwords with length >= 8)
  if (nchar(password) >= 8) {
    strength_score <- strength_score + 1
    values[1] <- 1
  }
  
  # Numbers (score +1 for the presence of numbers)
  if (grepl("[0-9]", password)) {
    strength_score <- strength_score + 1
    values[2] <- 1
  }
  
  # Uppercase (score +1 for the presence of uppercase letters)
  if (grepl("[A-Z]", password)) {
    strength_score <- strength_score + 1
    values[3] <- 1
  }
  
  # Special characters (score +1 for the presence of special characters)
  if (grepl("[[:punct:]]", password)) {
    strength_score <- strength_score + 1
    values[4] <- 1
  }
  
  # Determine strength category
  if (strength_score == 0) {
    strength_category <- "Very Weak"
  } else if (strength_score == 1) {
    strength_category <- "Weak"
  } else if (strength_score == 2) {
    strength_category <- "Medium"
  } else if (strength_score == 3) {
    strength_category <- "Strong"
  } else {
    strength_category <- "Very Strong"
  }
  
  return(list(strength_score = strength_score, strength_category = strength_category, values = values))
}

# Define UI
ui <- fluidPage(
  titlePanel("Password Strength Checker"),
  
  sidebarLayout(
    sidebarPanel(
      passwordInput("password", "Enter your password:", value = ""),
      actionButton("check_btn", "Check Password Strength")
    ),
    
    mainPanel(
      h4("Password Strength:"),
      textOutput("strength_text"),
      plotOutput("strength_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Observe the "Check Password Strength" button click
  observeEvent(input$check_btn, {
    password <- input$password
    result <- calculate_strength(password)
    
    # Display the strength category
    output$strength_text <- renderText({
      paste("Password strength is:", result$strength_category)
    })
    
    # Display the graphical representation of password criteria
    output$strength_plot <- renderPlot({
      data <- data.frame(Criteria = c("Length", "Numbers", "Uppercase", "Special Characters"),
                         Value = result$values)
      
      ggplot(data, aes(x = Criteria, y = Value, fill = Criteria)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("red", "orange", "yellow", "green")) +
        theme_minimal() +
        labs(title = "Password Strength Criteria", y = "Met Criteria (1 = Yes, 0 = No)") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
