#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####################################

# To Do:
#   Add in three numeric-based boxes/gauges.  # DONE
#   Try adding in scales to plots.
#   Update README.
#   Verify that data tables utilize reactivity.  # DONE
#   Refactor if there's time.
#   Make sure .gitignore is working.
#   Add descriptive comments, etc.
#   Add in "Add S" function, like before.  # DONE
#   Add in Dynamic titles to graphs (zip code, etc)
  
####################################



# load in the libraries I need
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(stringr)
library(ggplot2)
library(shinythemes)
# https://www.tutorialspoint.com/how-to-create-the-bar-chart-with-ggplot2-using-color-brewer-in-r
library(RColorBrewer)
library(scales)
library(DT)

# create some variables here

# https://stackoverflow.com/questions/69084375/extract-month-and-year-from-datetime-in-r
# data <- read_csv("dog_licenses.csv") %>%
#   mutate(reg_date = as.Date(reg_date, format="%m-%d-%Y")) %>%
#   mutate(reg_year = lubridate::year(reg_date)) %>%
#   na.omit()

data <- read_csv("dog_licenses.csv") %>%
  mutate(reg_year = as.Date(ValidDate, format="%m-%d-%Y")) %>%
  mutate(reg_year = format(as.Date(data$reg_year, format="%m-%d-%Y"), "%Y")) %>%
  na.omit()


yearWithMostRegistrations <- data %>%
  group_by(reg_year) %>%
  summarize(n = n()) %>%
  rename("num_dogs" = n) %>%
  slice_max(num_dogs)


mostPopularLicense <- data %>%
  group_by(type) %>%
  summarize(n = n()) %>%
  rename("num_licenses" = n) %>%
  slice_max(num_licenses)


mostPopularBreed <- data %>%
  group_by(breed) %>%
  summarize(n = n()) %>%
  rename("num_dogs" = n) %>%
  slice_max(num_dogs)


# ui stuff goes here
ui <- dashboardPage(
  
  # the title/heading that appears in the upper left of the app
  dashboardHeader(
    title = "Allegheny County Dog Licenses",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      id = "tabs",
      
      # the first "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one.
      menuItem(
        "Individual Breeds", 
        tabName = "plot1",
        icon = icon("chart-column")
      ),
      
      # the second "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one.
      menuItem(
        "Top Breeds by Zip Code", 
        tabName = "plot2",
        icon = icon("chart-column")
      ),
      
      # the third "page" of the app, seen in the upper left of the app
      # it corresponds with the tabItem that has the same tabName value as this one
      menuItem(
        "Licenses", 
        tabName = "plot3",
        icon = icon("chart-column")
      ),
      
      ################################
      ###  USER INPUTS BEGIN HERE  ###
      ################################
      
      # pick a date range (corresponds with the registration active date)
      dateRangeInput(
        inputId = "dates",
        label = "Dates:",
        start = "2012-01-01",
        end = Sys.Date(),
        min = "2012-01-01",
        max = Sys.Date()
      ),
      
      # pick a type of dog license
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
      
      # pick how many breeds to display at one time
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
      
      # pick a zip code, or two, or three, or...
      # might remove later
      selectInput(
        inputId = "zip",
        label = "Owner Zip Code",
        choices = unique(data$zip),
        selected = "15205",
        # multiple = TRUE,
        # selectize = TRUE,
        # options = list(create = FALSE)
      ),
      
      # pick some breeds
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
          "English Springer Spaniel" = "ENG SPRINGER SPANIE"
        ),
        selected = "BEAGLE"
      )
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # this is what the user sees when they click on the "Individual Breeds" tab
      tabItem(
        
        tabName = "plot1",
        
        # first row is the name of the page
        h2("Individual Breeds Breakdown"),
        
        # second row is value boxes, value boxes go here
        fluidRow(
          valueBoxOutput("mostPopularBreed1"),
          valueBoxOutput("mostPopularBreed2"),
          valueBoxOutput("mostPopularBreed3")
        ),
        
        # third row is the actual plot
        fluidRow(
          box(
            width = 12,
            plotlyOutput("licensesByBreed")
          )
        ),
        
        # fourth row is the data table corresponding to the plot
        fluidRow(
          box(
            width = 12,
            DT::dataTableOutput(
              outputId = "licensesByBreedTable",
            ),
          )
        )
      ),
      
      # this is what the user sees when they click on the "Top Breeds by Zip Code" tab
      tabItem(
        
        tabName = "plot2",
        
        # first row is the name of the page
        h2("Top Breeds By Zip Code"),
        
        # third row is the actual plot
        fluidRow(
          box(
            width = 12,
            plotlyOutput("topXDogs")
          )
        ),
        
        # fourth row is the data table corresponding to the plot
        fluidRow(
          box(
            width = 12,
            DT::dataTableOutput(
              outputId = "topXDogsTable",
            ),
          )
        )
      ),
      
      # this is what the user sees when they click on the "Licenses" tab
      tabItem(
        
        tabName = "plot3",
        
        # first row is the name of the page
        h2("Licenses"),
        
        # # second row is value boxes, value boxes go here
        # fluidRow(
        #   valueBoxOutput("mostPopularBreed1"),
        #   valueBoxOutput("mostPopularBreed2"),
        #   valueBoxOutput("mostPopularBreed3")
        # ),
        
        # third row is the actual plot
        fluidRow(
          box(
            width = 12,
            plotlyOutput("topLicenses")
          )
        ),
        
        # fourth row is the data table corresponding to the plot
        fluidRow(
          box(
            width = 12,
            # data table stuff
            DT::dataTableOutput(
              outputId = "licenseTable",
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
  
  
  ### helper function to add an "s" in the plot titles when needed ###
  add_s <- function(number) {
    if (number > 1) {
      return("s")
    } else {
      return("")
    }
  }
  
  
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
      filter(zip == input$zip) %>%
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
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots-1
          position = position_dodge()
        ) + 
        scale_fill_brewer(palette = "Set1") + 
        # MAKE SURE TO ADD THE "ADD_S" FUNCTION IN HERE
        ggtitle(paste(str_interp("Top ${input$top} Dog Breed${add_s(input$top)} By License Registration in Allegheny County's ${input$zip} Zip Code Over Time"))) +
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
        #   # labels = scales::label_date("'%y")
        # ) +
        geom_bar(
          alpha = 0.5,
          stat = "identity",
          # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization#create-barplots-1
          position = position_dodge()
        ) + 
        scale_fill_brewer(palette = "Set1") + 
        # MAKE SURE TO ADD THE "ADD_S" FUNCTION IN HERE
        ggtitle(paste(str_interp("Dog License Types in Allegheny County Over Time"))) +
        theme(plot.title = element_text(hjust = 0.5))
    )
  })
  
  
  # outputs the data table corresponding to the third plot
  output$licenseTable = DT::renderDataTable({
    DT::datatable(data = dhat3())
  })
  
  # the exact same box repeated three times, I know.
  # but this is a placeholder for now.
  # I'll probably make them static values of all-time most popular breed, year, and num_dogs
  output$mostPopularBreed1 <- renderValueBox(
    valueBox(
      h4(paste0(mostPopularBreed$breed)),
      str_interp("... is the all-time most popular breed in the county (there are a total of ${mostPopularBreed$num_dogs} individual dogs of this breed in the data set)."),
      icon = icon("list"),
      color = "purple"
    ),
  )
  
  output$mostPopularBreed2 <- renderValueBox(
    valueBox(
      h4(paste0(mostPopularLicense$type)),
      str_interp("... is the all-time most popular dog license in the county (there are a total of ${mostPopularLicense$num_licenses} individual licenses of this kind in the data set)."),
      icon = icon("list"),
      color = "purple"
    )
  )
  
  output$mostPopularBreed3 <- renderValueBox(
    valueBox(
      h4(paste0(yearWithMostRegistrations$reg_year)),
      str_interp("... was the single year with the most dog license registrations in the county (there were ${yearWithMostRegistrations$num_dogs} total registrations that year)."),
      icon = icon("list"),
      color = "purple"
    )
  )
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