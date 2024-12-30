# Load necessary library
library(shiny)

# Define a list of motivational and funny quotes
motivational_quotes <- c(
  "The only way to do great work is to love what you do. - Steve Jobs",
  "Success is not final, failure is not fatal: It is the courage to continue that counts. - Winston Churchill",
  "The harder you work for something, the greater you’ll feel when you achieve it. - Unknown",
  "Don’t watch the clock; do what it does. Keep going. - Sam Levenson"
)

funny_quotes <- c(
  "I am on a seafood diet. I see food, and I eat it. - Unknown",
  "I told my computer I needed a break, and now it won’t stop sending me Kit-Kats. - Unknown",
  "Behind every great man is a woman rolling her eyes. - Jim Carrey",
  "Why don’t skeletons fight each other? They don’t have the guts. - Unknown"
)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Random Quote Generator"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select quote category
      selectInput("category", "Select Quote Category:", 
                  choices = c("Motivational", "Funny")),
      # Button to generate a random quote
      actionButton("generate", "Generate Random Quote"),
      br(),
      textOutput("quote")  # Display the quote
    ),
    
    mainPanel(
      h3("Your Random Quote Will Appear Here!")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to generate the quote based on selected category
  quote_text <- eventReactive(input$generate, {
    if (input$category == "Motivational") {
      # Random quote from motivational quotes
      sample(motivational_quotes, 1)
    } else {
      # Random quote from funny quotes
      sample(funny_quotes, 1)
    }
  })
  
  # Render the selected quote
  output$quote <- renderText({
    quote_text()
  })
}

# Run the application
shinyApp(ui = ui, server = server)