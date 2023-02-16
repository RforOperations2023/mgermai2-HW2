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
library(ggplot2)
library(shinythemes)

# create some variables here

# https://stackoverflow.com/questions/69084375/extract-month-and-year-from-datetime-in-r
data <- read_csv("dog_licenses.csv") %>%
  mutate(reg_date = as.Date(reg_date, format="%m-%d-%Y")) %>%
  mutate(reg_year = lubridate::year(reg_date))

# ui stuff goes here
ui <- dashboardPage(
  dashboardHeader(
    title = "Allegheny County Dog Licenses"
  ),
  dashboardSidebar(
    
    sidebarMenu(
      
      # helpText("TABS", width = "100%"),
      
      id = "tabs",
      
      menuItem(
        "Plot #1", 
        tabName = "plot1",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Plot #2", 
        tabName = "plot2",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Plot #3", 
        tabName = "plot3",
        icon = icon("chart-column")
      ),
      
      helpText("INPUTS", width = "100%"),
      
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
      
      numericInput(
        inputId = "top",
        label = "Top X Breeds",
        value = 2,
        min = 1,
        max = 5,
        step = 1
        # width = NULL
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
        ),
        selected = "BEAGLE"
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "plot1",
        h2("Plot #1"),
        fluidRow(
          box(
            title = h3(strong("Dog Licenses by Breed Over Time")),
            wdith = 12,
            br(),
            # need value box output here
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotOutput("licensesByBreed")
          )
        )
      ),
      
      tabItem(
        tabName = "plot2",
        h2("Plot #2"),
        fluidRow(
          box(
            title = h3(strong("Top Dog Breeds")),
            wdith = 12,
            br(),
            # need value box output here
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotOutput("topXDogs")
          )
        )
      ),
      
      tabItem(
        tabName = "plot3",
        h2("Plot #3")
      )
      
    )
    
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
  
  # first data set
  dhat <- reactive({
    result = data %>%
      filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
      # filter(zip %in% input$zip) %>%
      filter(breed %in% input$breed) %>%
      # arrange(reg_year) %>%
      group_by(reg_year, breed) %>%
      summarize(n = n()) %>%
      rename("num_dogs" = n)
    print("---dhat"); return(result);
  })
  
  # second data set
  dhat2 <- reactive({
    result = data %>%
      filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
      filter(zip %in% input$zip) %>%
      group_by(reg_year, breed) %>%
      summarise(n = n()) %>%
      rename("num_dogs" = n) %>%
      slice_max(order_by = num_dogs, n = input$top)
    print("---dhat2"); return(result);
  })
  
  
  output$licensesByBreed <- renderPlot({
    dhat() %>%
      ggplot(
        mapping = aes(
          x = reg_year, 
          y = num_dogs, 
          color = breed,
        )
      ) +
      xlab("Year of License Registration") +
      ylab("Number of Individual Dogs") +
      geom_line(alpha = 0.5) + 
      geom_point(alpha = 0.5) +
      ggtitle(paste(str_interp("Allegheny County Dog License Registration Over Time (by Breed)"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$topXDogs <- renderPlot({
    dhat2() %>%
      ggplot(
        mapping = aes(
          x = reg_year, 
          y = num_dogs, 
          # would definitely be helpful to change the color so it's easier to read
          fill = breed,
        )
      ) +
      xlab("Year of License Registration") +
      ylab("Number of Individual Dogs") +
      geom_bar(
        alpha = 0.5,
        stat = "identity",
        # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots-1
        position = position_dodge()
      ) + 
      # geom_point(alpha = 0.5) +
      # MAKE SURE TO ADD THE "ADD_S" FUNCTION IN HERE
      ggtitle(paste(str_interp("Top 'X' Dog Breeds By Registration Over Time"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  # output$licensesByBreed <- renderPlotly(
  #   dhat() %>%
  #     ggplot(
  #       mapping = aes(
  #         x = reg_year,
  #         y = num_dogs,
  #         color = breed,
  #       )
  #     ) +
  #       geom_line()
  # )
  
  # gets counts of breeds throughout the county over time
  # dhat <- data %>%
  #   filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  #   # filter(zip %in% input$zip) %>%
  #   filter(breed %in% input$breed) %>%
  #   arrange(reg_year) %>%
  #   group_by(reg_year, breed) %>%
  #   summarize(n = n()) %>%
  #   rename("num_dogs" = n)
  # 
  # 
  # # maybe make a new graph for each zip code?
  # # this will get the top x breeds over a period of time in a single zip code
  # dhat2 <- data %>%
  #   filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  #   filter(zip %in% input$zip) %>%
  #   group_by(reg_year, breed) %>%
  #   summarise(n = n()) %>%
  #   rename("num_dogs" = n) %>%
  #   slice_max(order_by = num_dogs, n = input$top)
  # 
  # # gets the count of types of licenses throughout the county over time
  # dhat3 <- data %>%
  #   filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  #   filter(type %in% input$type) %>%
  #   group_by(reg_year, type) %>%
  #   summarize(n = n()) %>%
  #   rename("num_licenses" = n)
  
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