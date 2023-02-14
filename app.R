#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load in the libraries I need
library(shiny)
library(shinydashboard)

# create some variables here

# BREEDS -- STATIC VALUE
# go through each file and find the unique breeds
years = 

# ui stuff goes here
ui <- dashboardPage(
  dashboardHeader(
    # TBD
  ),
  dashboardSidebar(
    
    dateRangeInput(
      inputId = "dates",
      label = "Dates:",
      start = "2007-01-01",
      end = Sys.Date(),
      min = "2007-01-01",
      max = Sys.Date()
    ),
    
    
    
  ),
  dashboardBody(
    # TBD
  )
)

server <- function(input, output) {
  # TBD
  #
  # okay, so here are some charts I'm thinking of making:
  #
  # 1. Top "X" breeds by user-input zip code -- bar graph
  #    (user selects the "X" breeds and the zip code(s))
  # 2. Count of "Y" Breeds over time (popularity, etc.) -- line graph
  #    (user selects the years and the breeds)
  # 3. Count of individual types of licenses over time -- line graph
  #    (user selects the years and the types of licenses)
}

# run the application 
shinyApp(ui = ui, server = server)


# IDEAS FOR CHARTS TO MAKE

# Color of dog
# Count of each breed by year (bar plot)
# Count of each breed (bar plot)
# Count of each breed by zip code (bar plot)
# Pre-process the data


# look into ggplotly again
# ignore any warnings whenever ggplot says it doesn't recognize any plotly commands