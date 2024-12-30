# Load necessary libraries
library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Shape Drawer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "shape", 
        label = "Choose a shape:", 
        choices = c("Circle", "Square", "Triangle"),
        selected = "Circle"
      ),
      numericInput(
        inputId = "size", 
        label = "Size (radius/side length):", 
        value = 5, 
        min = 1, 
        step = 0.1
      ),
      actionButton(inputId = "draw", label = "Draw Shape")
    ),
    
    mainPanel(
      plotOutput("shapePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to draw the shape
  output$shapePlot <- renderPlot({
    input$draw  # Trigger plot update when the draw button is pressed
    
    shape <- input$shape
    size <- input$size
    
    # Set up a blank plot
    p <- ggplot() + 
      coord_fixed(ratio = 1) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
    
    if (shape == "Circle") {
      p <- p + annotate("path", 
                        x = size * cos(seq(0, 2*pi, length.out = 100)), 
                        y = size * sin(seq(0, 2*pi, length.out = 100)), 
                        color = "blue")
      
    } else if (shape == "Square") {
      p <- p + geom_rect(aes(xmin = -size/2, xmax = size/2, ymin = -size/2, ymax = size/2), fill = "green")
      
    } else if (shape == "Triangle") {
      p <- p + geom_polygon(aes(x = c(-size, 0, size), y = c(-size/2, size, -size/2)), fill = "red")
    }
    
    # Display the shape
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)
