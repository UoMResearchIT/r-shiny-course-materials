#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

gapminder <- readRDS("gapminder.rds")
source("workshopFunctions.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gapminder plot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         checkboxGroupInput("continent",
                            "Select continents",
                            choices = levels(gapminder$continent),
                            selected = levels(gapminder$continent))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("gapminderPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$gapminderPlot <- renderPlot({
      gapminder %>% 
       filter(year == 2000) %>% 
       produceGapminderPlot()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

