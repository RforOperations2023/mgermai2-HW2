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

# ui stuff goes here
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) {
  # TBD
}

# run the application 
shinyApp(ui = ui, server = server)
