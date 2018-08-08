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
  
   fluidRow(
      column(12,
         plotOutput("gapminderPlot", click="plotClick"),
         tableOutput("graphData"),
         verbatimTextOutput("clickData")
      )
   ),
   fluidRow(
      column(6,
             sliderInput("year",
                         "Select year",
                         min = min(gapminder$year),
                         max = max(gapminder$year),
                         value = min(gapminder$year),
                         sep="", # Hide thousands , separator
                         step=1, # Select every year
                         animate = animationOptions(interval = 1250)
             )
      ),
      column(6,
             checkboxGroupInput("continent",
                                "Select continents",
                                choices = levels(gapminder$continent),
                                selected = levels(gapminder$continent))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # Create a reactive value to store the country we seleect
   activeCountry <- reactiveVal()
   
   historicData <- reactive({
      gapminder %>% 
         filter(year <= input$year) %>% 
         filter(country == activeCountry()) # activeCountry is reactive, its value is accessed using activeCountry()
   })
   
   # Update the value of activeCountry() when we detect an input$plotClick event
   # (Note how we update a reactiveVal() )
   observeEvent(input$plotClick, 
                {
                   nearCountry <- nearPoints(plotData(), input$plotClick, maxpoints = 1)
                   activeCountry(as.character(nearCountry$country)) # Extract just the country name and assign it to activeCountry()
                })
   
   plotData <- reactive({
     gapminder %>% 
       filter(year == input$year) %>% 
       filter(continent %in% input$continent)
   })
   
   output$graphData <- renderTable({
     plotData() %>% 
       getRichestCountry()
   })
   
   output$gapminderPlot <- renderPlot({
      if (length(activeCountry()) == 0) { # No country selected; regular plot
         plotData() %>% 
            produceGapminderPlot() 
      } else {
         plotData() %>% 
            produceGapminderPlot(makeSemiTransparent = TRUE) %>% 
            addHistoricPlot(historicData())
      }
   })
   
   output$clickData <- renderPrint(({
      activeCountry()
   }))
}

# Run the application 
shinyApp(ui = ui, server = server)

