# Functions used in the Workshop

#' Plot a single year of gapminder data, passed in indata.
#' makeSemiTransparent will make the points semi transparent;
#' this  produces a clearer graph if we overlay historic data on it
produceGapminderPlot <- function(indata, 
                                 makeSemiTransparent = FALSE,
                                 x = gdpPerCap,
                                 y = lifeExp){
  x <- ensym(x)
  y <- ensym(y)
  
  # These are taken from the gapminder package, but made brighter
  continent_colours <-c(Africa = "#BF590CFF", Americas = "#F80039FF", Asia = "#600071FF", 
                        Europe = "#3B9626FF", Oceania = "#4A51E0FF")
  
  # Set nice axes for the data
  # We can't do this directly from the data since the limits would change
  # for each year.
  axislimits <- list(tfr = c(0.5, 9),
                     lifeExp = c(20, 88),
                     gdpPerCap = c(250, 60000))
  
  axislabels <- list(tfr = "Total Fertility Rate",
                     lifeExp = "Life Expectancy (years)",
                     gdpPerCap = "GDP per capita (ppp dollars)")
  
  if(!all(names(axislimits) %in% names(axislabels)) || 
     !all(names(axislabels) %in% names(axislimits)) ){
    stop("Haven't defined the axis limits and labels for the same variables")
  }
  
  if(!(toString(x) %in% names(indata))) {
    stop(paste(toString(x), "not found in input data"))
  }
  
  if(!(toString(y) %in% names(indata))) {
    stop(paste(toString(y), "not found in input data"))
  }
  
  if(!(toString(x) %in% names(axislimits))) {
    stop(paste("Don't know how to handle the axis/labels for", toString(x)))
  }
  
  if(!(toString(y) %in% names(axislimits))) {
    stop(paste("Don't know how to handle the axis/labels for", toString(y)))
  }
  
  plotYear <- min(indata$year)
  
  if (nrow(indata) > 0 && max(indata$year) != plotYear) {
    stop("More than one year of data passed to plotting function - use filter(year == xxxx)")
  }
  
  
  # Set up the plot window, with appropriate limits
  gapminderPlot <- ggplot(data = indata) +
    coord_cartesian(xlim = axislimits[[toString(x)]],
                    ylim = axislimits[[toString(y)]]) 
  
  # We need to log scale gdpPerCap
  if(toString(x) == "gdpPerCap"){
    gapminderPlot <- gapminderPlot +
      scale_x_log10(breaks = 4*10**(2:4)) 
  }
  
  if(toString(y) == "gdpPerCap"){
    gapminderPlot <- gapminderPlot +
      scale_y_log10(breaks = 4*10**(2:4)) 
  }
  
  # Return the empty plot if we've no data
  if (nrow(indata) == 0){
    return(gapminderPlot)
  }
  
  
  if (makeSemiTransparent) {
    pointAlpha <- 0.3
  } else {
    pointAlpha <- 1
  }
  
  ## Plot the data
  gapminderPlot <- gapminderPlot +  
    geom_point(aes(x = !!x,
                   y = !!y, 
                   colour = continent,
                   size = population),
               alpha = pointAlpha) + 
    labs(title = plotYear,
         x = axislabels[[toString(x)]],
         y = axislabels[[toString(y)]],
         colour = "Continent") +
    scale_colour_manual(values = continent_colours) +
    scale_alpha_manual() +
    guides(size = FALSE) 
  
  return(gapminderPlot)
  
}


# Get the variable name plotted on an axis from a ggplot object
# We use this function to make sure we plot the historic data for the
# same variables as the main plot
getAxis <- function(g, axis) {
  
  if( length(g$layers) > 1 ){
    stop("Only implemented for single layer graphs")
  } 
  
  if ( !(axis %in% c("x","y")) ) {
    stop("Invalid axis specified")
  }
  
  return(rlang:::get_expr(g$layers[[1]]$mapping[[axis]]))
  
}

# Add historic data to a "gapminder" plot.
# This function expects a single country's data
addHistoricPlot <- function(inplot, indata, activeYear = NULL){
  
  if (length(unique(indata$country)) > 1 ) {
    stop("More than one county's data passed")
  }
  
  if (is.null(activeYear)) {
    activeYear = max(indata$year)  
  }  
  
  xdata = getAxis(inplot, "x")
  ydata = getAxis(inplot, "y")
  
  outplot <- inplot + geom_path(data = indata,
                                aes(x = !!xdata,
                                    y = !!ydata,
                                    colour = continent),
                                show.legend = FALSE) +
    geom_point(data = indata[indata$year == activeYear,],
               aes(x = !!xdata,
                   y = !!ydata,
                   colour = continent,
                   size = population),
               show.legend = FALSE)
  
  return(outplot)
  
}

# get the richest country in a tibble, and return
# the country and its GDP as a tibble
getRichestCountry <- function(indata) {

  if (max(indata$year) != min(indata$year) ) {
    warning("More than one year of data passed in")
  }
  
  outdata <- indata %>% 
    arrange(desc(gdpPerCap)) %>% 
    mutate(rn = row_number()) %>% 
    filter(rn == 1) %>% 
    select(country, gdpPerCap)
  
  return(outdata)
  
}
