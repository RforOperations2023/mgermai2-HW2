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
library(tidyverse)
library(plotly)
library(stringr)

# create some variables here

# data <- read_csv("dog_licenses.csv") %>%
#   mutate(date = as.Date(date, "%Y-%m-%d"))

# ui stuff goes here
ui <- dashboardPage(
  dashboardHeader(
    # TBD
  ),
  dashboardSidebar(
    
    # pick a date range (corresponds with the registration active date)
    dateRangeInput(
      inputId = "dates",
      label = "Dates:",
      start = "2012-01-01",
      end = Sys.Date(),
      min = "2012-01-01",
      max = Sys.Date()
    ),

    selectizeInput(
      inputId = "types",
      label = "Type of License:",
      choices = c(
        "Neutered Male" = "Dog Individual Neutered Male",
        "Male" = "Dog Individual Male",
        "Senior/Disabled Spayed Female" = "Dog Senior Citizen or Disability Spayed Female",
        "Senior/Disabled Neutered Male" = "Dog Senior Citizen or Disability Neutered Male",
        "Spayed Female" = "Dog Individual Spayed Female",
        "Female" = "Dog Individual Female",
        "Senior/Disabled Male" = "Dog Senior Citizen or Disability Male",
        "Senior/Disabled Female" = "Dog Senior Citizen or Disability Female",
        "Inter County Transfer" = "Dog Inter County Transfer",
        "Inter County Transfer - FREE" = "Dog Inter County Transfer - FREE",
        "Out of County Transfer - Senior" = "Dog Out of County Transfer - Senior",
        "Out of County Transfer" = "Dog Out of County Transfer",
        "Individual License - FREE" = "Dog Individual License - FREE",
        "Senior/Disabled License - FREE" = "Dog Senior Citizen or Disability - FREE",
        "Individual License Duplicate" = "Dog Individual License Duplicate",
        "Senior/Disabled License Duplicate" = "Dog Senior Citizen or Disability Duplicate"
      ),
      selected = "Dog Senior Citizen or Disability Duplicate",
      multiple = TRUE,
      options = list(create = FALSE)
    ),

    selectizeInput(
      inputId = "zip",
      label = "Owner Zip Code",
      choices = unique(data$zip),
      selected = "15205",
      multiple = TRUE,
      # selectize = TRUE,
      options = list(create = FALSE)
    ),
    
    checkboxGroupInput(
      inputId = "breed",
      label = "Breed:",
      choices = c(
        "Labrador Retriever" = "LABRADOR RETRIEVER", 
        "Golden Retriever" = "GOLDEN RETRIEVER", 
        "German Shepherd" = "GER SHEPHERD",
        "Shih Tzu" = "SHIH TZU",
        "Beagle" = "BEAGLE",
        "Chihuahua" = "CHIHUAHUA",
        "American Pit Bull Terrier" = "AM PIT BULL TERRIER",
        "Yorkshire Terrier" = "YORKSHIRE TERRIER",
        "Dachsund" = "DACHSHUND",
        "Boxer" = "BOXER",
        "Pug" = "PUG",
        "Cocker Spaniel" = "COCKER SPANIEL",
        "Bichon Frese" = "BICHON FRISE",
        "Jack Russell Terrier" = "PARSON RUSSELL TERR",
        "Maltese" = "MALTESE",
        "Pomeranian" = "POMERANIAN",
        "Pembroke Welsh Corgi" = "WELSH CORGI PEMBROK",
        "Cardigan Welsh Corgi" = "WELSH CORGI CARDIGA",
        "Brittany Spaniel" = "BRITTANY SPANIEL",
        "Springer Spaniel" = "SPRINGER SPANIE"
      )
    )
    
    
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