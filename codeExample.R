# This script contains some (non Shiny) code illustrating how to use the functions provided


library(ggplot2)
library(dplyr)
library(shiny)

# Load the gapminder data 
# (this contains more data than that included in the gapminder R package)
# Note that when we run a shiny app, the working directory is the app.R's location
gapminder <- readRDS("gapminder.rds")

# Load the helper functions we'll use for plotting.
source("workshopFunctions.R")

# Preview the data
print(gapminder)

# Country and continent are factors
# You can get a vector of the factor's levels with:
levels(gapminder$continent)

# Produce a plot for a year of data

# produceGapminderPlot expects a single year of data
# Using pipes to do this:
gapminder %>% 
  filter(year == 2000) %>% 
  produceGapminderPlot()

# We can add further filters to the pipe, e.g.
# to only show certain continents:
gapminder %>% 
  filter(year == 2000) %>% 
  filter(continent %in% c("Europe", "Africa")) %>% 
  produceGapminderPlot()

#########################################################################
# The material below is for the "going further" section of the workshop #
#########################################################################

# We can return a tibble showing the richest country and its GDP per capita
# using the getRichestCountry() function:
gapminder %>%
  filter(year == 1800) %>% 
  getRichestCountry()

# When we are producing a plot with a historic trace for a country, we 
# need to create a data set with that country's historic data:
historicdata <- gapminder %>% 
  filter(year <= 2000) %>% 
  filter(country == "United Kingdom")

# addHistoricPlot() will add a trace showing the trajectory of a country
# to an exising plot:
gapminder %>% 
  filter(year == 2000) %>% 
  produceGapminderPlot() %>% 
  addHistoricPlot(historicdata)

# You probably want to set the makeSemiTransparent option 
# on produceGapminderPlot() to focus attention on the selected country
gapminder %>% 
  filter(year == 2000) %>% # Only show the trace up to the year we're plotting
  produceGapminderPlot(makeSemiTransparent = TRUE) %>% 
  addHistoricPlot(historicdata) 


