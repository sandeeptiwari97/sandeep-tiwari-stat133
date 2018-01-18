#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library('ggplot2')
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Grade Visualizer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("X-axis variable", "X-axis variable", colnames(scores)[1:21]),
         sliderInput("bins",
                     "Bin Width",
                     min = 1,
                     max = 10,
                     value = 10)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot")
        tabsetPanel(
          tabPanel("Barchart", plotOutput("plot")),
          tabPanel("Histogram", plotOutput("histogram")),
          tabPanel("Scatterplot", plotOutput("scatterplot"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
   output$plot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- scores[, 5] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      barplot(x, col = 'darkgray', border = 'white')
   })
   output$histogram <- renderPlot({
     x <- scores[, 6]
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     hist(x, col = 'darkgray', border = 'white')
   })
   output$scatterplot <- renderPlot({
     plot(scores$HW1, scores$HW2)
   })
}

# Run the application 
shinyApp(ui = ui, server)

