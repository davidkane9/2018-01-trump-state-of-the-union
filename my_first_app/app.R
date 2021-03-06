#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("State of the Union Sentiment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     
       read_rds("saved.rds") %>% 
       ggplot(aes(x = date, y = rating, color = party)) + geom_point() +
       xlab("Date") +
       ylab("Average Sentiment Score using AFINN Dictionary") +
       ggtitle("State of the Union Sentiment Scores")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

