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
library(RColorBrewer)
library(scales)
library(DT)

# create some variables here

# https://stackoverflow.com/questions/69084375/extract-month-and-year-from-datetime-in-r
data <- read_csv("dog_licenses.csv") %>%
  mutate(reg_date = as.Date(reg_date, format="%m-%d-%Y")) %>%
  mutate(reg_year = lubridate::year(reg_date))

# ui stuff goes here
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Allegheny County Dog Licenses",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      id = "tabs",
      
      menuItem(
        "Individual Breeds", 
        tabName = "plot1",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Top Breeds by Zip Code", 
        tabName = "plot2",
        icon = icon("chart-column")
      ),
      
      menuItem(
        "Licenses", 
        tabName = "plot3",
        icon = icon("chart-column")
      ),
      
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
        inputId = "type",
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
        # this is funky/does not work in studio, but works in the browser:  https://github.com/rstudio/shiny/issues/3340
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
        h2("Individual Breeds"),
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
            plotlyOutput("licensesByBreed")
          )
        ),
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "licensesByBreedTable",
            ),
          )
        )
      ),
      
      tabItem(
        tabName = "plot2",
        h2("Top Breeds By Zip Code"),
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
            plotlyOutput("topXDogs")
          )
        ),
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "topXDogsTable",
            ),
          )
        )
      ),
      
      tabItem(
        tabName = "plot3",
        h2("Licenses"),
        fluidRow(
          box(
            title = h3(strong("Top Dog Licenses (by Type)")),
            wdith = 12,
            br(),
            # need value box output here
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("topLicenses")
          )
        ),
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "licensesTable",
            ),
          )
        )
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
  
  # third data set
  dhat3 <- reactive({
    result = data %>%
      filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
      filter(type %in% input$type) %>%
      group_by(reg_year, type) %>%
      summarize(n = n()) %>%
      rename("num_licenses" = n)
    print("---dhat3"); return(result);
  })
  
  
  output$licensesByBreed <- renderPlotly({
    ggplotly(
      dhat() %>%
        ggplot(
          mapping = aes(
            x = reg_year, 
            y = num_dogs, 
            color = breed,
          )
        ) +
        labs(
          x = "Year of License Registration",
          y = "Number of Individual Dogs",
          color = "Breed of Dog"
        ) +
        # scale_x_date(
        #   NULL,
        #   breaks = scales::breaks_width("2 years"), 
        #   labels = scales::label_date("'%y")
        # ) + 
        scale_color_brewer(palette = "Set1") + 
        geom_line(alpha = 0.5) + 
        geom_point(alpha = 0.5) +
        ggtitle(paste(str_interp("Allegheny County Dog License Registration Over Time (by Breed)"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  })
  
  
  # outputs the data table corresponding to the third plot
  output$licensesByBreedTable = DT::renderDataTable({
    DT::datatable(data = dhat())
  })
  
  
  output$topXDogs <- renderPlotly({
    ggplotly(
      dhat2() %>%
        ggplot(
          mapping = aes(
            x = reg_year, 
            y = num_dogs,
            fill = breed
          )
        ) +
        labs(
          x = "Year of License Registration",
          y = "Number of Individual Dogs",
          fill = "Breed of Dog"
        ) +
        # scale_x_date(
        #   NULL,
        #   breaks = scales::breaks_width("2 years"), 
        #   labels = scales::label_date("'%y")
        # ) + 
        scale_color_brewer(palette = "Set1") + 
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots-1
          position = position_dodge()
        ) + 
        # MAKE SURE TO ADD THE "ADD_S" FUNCTION IN HERE
        ggtitle(paste(str_interp("Top 'X' Dog Breeds By License Registration in Allegheny County Over Time"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  })
  
  
  # outputs the data table corresponding to the third plot
  output$topXDogsTable = DT::renderDataTable({
    DT::datatable(data = dhat2())
  })
  
  
  output$topLicenses <- renderPlotly({
    ggplotly(
      dhat3() %>%
        ggplot(
          mapping = aes(
            x = reg_year, 
            y = num_licenses,
            fill = type
          )
        ) +
        # https://www.geeksforgeeks.org/how-to-change-legend-title-in-ggplot2-in-r/
        labs(
          x = "Year of License Registration",
          y = "Number of Licenses",
          fill = "Type of Dog License"
        ) +
        # scale_x_date(
        #   NULL,
        #   breaks = scales::breaks_width("2 years"), 
        #   labels = scales::label_date("'%y")
        # ) + 
        scale_color_brewer(palette = "Set1") + 
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots-1
          position = position_dodge()
        ) + 
        # MAKE SURE TO ADD THE "ADD_S" FUNCTION IN HERE
        ggtitle(paste(str_interp("Top Dog License Types in Allegheny County Over Time"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  })
  
  
  # outputs the data table corresponding to the third plot
  output$licensesTables = DT::renderDataTable({
    DT::datatable(data = dhat3())
  })
  
  
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