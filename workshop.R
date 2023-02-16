#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load in the libraries I need
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(lubridate)
library(vroom)

# https://stackoverflow.com/questions/69084375/extract-month-and-year-from-datetime-in-r
data <- read_csv("data/allegheny_county_dog_licenses_2022.csv") %>%
  mutate(reg_date = as.Date(ValidDate, format="%m-%d-%Y")) %>%
  mutate(reg_year = lubridate::year(ValidDate))




spaniels <- read_csv("./spaniels.csv")
# new_spaniels <- spaniels[]









input = list(
  # Date thresholds
  dates = c("2012-01-01", "2023-02-16"),
  top = 5,
  zip = c(15205),
  type = c("Dog Individual Spayed Female", "Dog Individual Neutered Male"),
  breed = c("ENG SPRINGER SPANIE")
)

output = list()


spaniels <- data %>%
  filter(Breed %in% input$breed)

write.csv(
  spaniels[,-8:-9],
  file = "spaniels_cleaned.csv",
  col.names = FALSE,
  row.names = FALSE,
  quote = FALSE
)
  




  group_by(breed) %>%
  summarize(n = n()) %>%
  rename("num_dogs" = n)
  # slice_max(num_dogs)




straightData <- data %>%
  group_by(reg_year) %>%
  summarize(n = n()) %>%
  rename("num_dogs" = n) %>%
  slice_max(num_dogs)
  # summarise(reg_year, max = max(num_dogs))
  # slice(1)

print(straightData)
  




# gets counts of breeds throughout the county over time
dhat <- data %>%
  filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  # filter(zip %in% input$zip) %>%
  filter(breed %in% input$breed) %>%
  # arrange(reg_year) %>%
  group_by(reg_year, breed) %>%
  summarize(n = n()) %>%
  rename("num_dogs" = n)
  

dhatZip <- data %>%
  filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  filter(zip %in% input$zip) %>%
  group_by(zip, reg_year, breed) %>%
  summarise(n = n()) %>%
  rename("num_dogs" = n) %>%
  slice_max(order_by = num_dogs, n = input$top)
  
for (zipCode in input$zip) {
  newData <- dhatZip %>%
    filter(zip == zipCode)
  print(newData)
}

print(input$zip)

# maybe make a new graph for each zip code?
# this will get the top x breeds over a period of time in a single zip code
dhat2 <- data %>%
  filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  filter(zip %in% input$zip) %>%
  group_by(zip, reg_year, breed) %>%
  summarise(n = n()) %>%
  rename("num_dogs" = n) %>%
  slice_max(order_by = num_dogs, n = input$top)
  

# gets the count of types of licenses throughout the county over time
dhat3 <- data %>%
  filter(reg_date >= input$dates[1], reg_date < input$dates[2] ) %>%
  filter(type %in% input$type) %>%
  group_by(reg_year, type) %>%
  summarize(n = n()) %>%
  rename("num_licenses" = n)

  


group_by(reg_year, zip, breed, num_dogs)
  # arrange(desc(num_dogs)) %>%
  # ungroup() %>%
  # group_by(reg_year, zip, breed) %>%
  slice_max(order_by = num_dogs, n = 5)
  # ungroup()
  
  # ungroup() %>%
  
  # group_by(reg_year, zip, breed) %>%
  slice(1:input$top)

  
  # filter(breed %in% input$breed) %>%
  arrange(reg_year) %>%
  group_by(reg_year, breed) %>%
  count() %>%
  rename("num_dogs" = n)

  
  
# %>%
#   count(reg_year)
  # group_by(calendar_year = lubridate::floor_date(date, "year")) %>%
  # summarise(breed=n())



%>%
  # mutate(id = paste(route, day_type, sep = "-")) %>%
  select(date, zip, breed)


# problems(data)
# 
# head(data)
# 
# 
# data <- readRDS("dog_licenses.rds")
# 
# head(data)
# 
# 
# data <- readRDS("dog_licenses.rds") %>%
#   mutate(month_start = as.Date(date, "%Y-%m-%d"))
# 
# 
# 
# 
# data = "data/allegheny_county_dog_licenses_2015.csv" %>% 
#   read_csv()
# 
# 
# head(data)



data %>%
  group_by(Breed) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  print(n = 25)
  # ungroup() %>% 
  # filter(breed %in% c("BEAGLE", "CHIHUAHUA", "WELSH CORGI PEMBROK",
  #                     "WELSH CORGI CARDIGA", "MIXED"))


# 
# types = list()
# 
get_types = function(year) {
  dir("data", pattern = paste(year), full.names = TRUE)   %>%
    read_csv() %>%
    unique(df[c("OwnerZip")])
}

get_types(2010)

for (i in years) {
  print(get_types(i))
}



years = list(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

for (i in years) {
  df = 'data/allegheny_county_dog_licenses_2022.csv' %>%
    read_csv()
  
  # head(df)
  
  unique(df[c("OwnerZip")])
}


file.names <- list.files(path = "data", recursive = TRUE,
                         pattern = "\\.csv$", full.names = TRUE)

file.names


mylist <- list()

for(i in 1:length(file.names)){
  print(i)
  df <- read.csv(file.names[i], sep=",", stringsAsFactors=FALSE)
  listtmp = unique(df[c("OwnerZip")])
  for (zip in listtmp) {
    mylist <- append(mylist, zip)
  }
  # mylist <- append(mylist, list(listtmp))
}

df = unique(mylist)

head(df)


newlist = unique(df[c("OwnerZip")])


mybreeds = c("BEAGLE", "CHIHUAHUA", "WELSH CORGI PEMBROK",
         "WELSH CORGI CARDIGA", "MIXED")

# function to be used later...
get_file = function(year){
  dir("data", pattern = paste(year), full.names = TRUE)   %>%
    read_csv() %>%
    select(
      type = LicenseType,
      breed = Breed,
      color = Color,
      name = DogName,
      zip = OwnerZip, 
      year = ExpYear, 
      date = ValidDate
    ) %>%
    # filter by the breed that the user (eventually) selects...
    # except "mybreeds" is static for now.
    filter(breed %in% mybreeds)
}

# important that this is a list, even if it's a list containing only one value.
# input = list(
#   year = 2013,
#   year = 2014
# )

input = c(2013, 2014)

# okay, so this reads in the year(s) that the "user" input and filters out the relevant data.
head(get_file(year = input$year))
tail(get_file(year = input$year))



# mybreeds = c("BEAGLE")
# 
# years = c(2007)
# 
# for(i in years){
#   print(get_file(2007))
# }

# data = "data/allegheny_count_dog_licenses_2015.csv" %>% read_csv()

# data = "data/allegheny_county_dog_licenses_2015.csv" %>%
#   read_csv() %>%
#   select(
#     type = LicenseType,
#     breed = Breed,
#     color = Color,
#     name = DogName,
#     zip = OwnerZip,
#     year = ExpYear,
#     date = ValidDate
#   ) %>%
#   # filter by the breed that the user (eventually) selects...
#   # except "mybreeds" is static for now.
#   filter(breed %in% mybreeds)




# Get a bunch of paths
# paths = dir("data", full.names = TRUE)
# 
# print(paths)

# # function to be used later...
# get_file = function(year){
#   dir("data", pattern = paste(year), full.names = TRUE)   %>%
#     read_csv() %>%
#     select(
#       type = LicenseType,
#       breed = Breed,
#       color = Color,
#       name = DogName,
#       zip = OwnerZip, 
#       year = ExpYear, 
#       date = ValidDate
#     ) %>%
#     # filter by the breed that the user (eventually) selects...
#     # except "mybreeds" is static for now.
#     filter(breed %in% mybreeds)
# }

# # important that this is a list, even if it's a list containing only one value.
# input = list(
#   year = 2013
# )
# 
# # okay, so this reads in the year(s) that the "user" input and filters out the relevant data.
# get_file(year = input$year)

print(Sys.Date())

# # Example Function
# for(i in 1:10){
#   paths[i] %>% read_csv() %>% head() %>% print()
# }


# selectInput(
#   #  choices = c("Tim" = "tim", "Matt" = "matt")
#   choices = c("2012" = "data/.........2012.csv", ...)
# )

# get_file = function(year){
#   dir("data", pattern = paste(year), full.names = TRUE)   %>%
#     read_csv() %>%
#     select(type = LicenseType, breed = Breed, color = Color, name = DogName, 
#            zip = OwnerZip, year = ExpYear, date = ValidDate) %>%
#     filter(breed %in% mybreeds)
# }

# input = list(
#   year = 2012
# )
# 
# get_file(year = input$year)
# 
# c(2002, 2003, 2004)
# 2002:2004
# c(2002:2004)

# superdata = 2010:2017 %>%
#   map_dfr(~get_file(year = .))
# 
# superdata %>% head()
# superdata %>% tail()
# 
# superdata %>% saveRDS("data.rds")
# superdata %>% write_csv("data.csv")
# 
# read_rds("data.rds") %>% system.time()
# read_csv("data.csv") %>% system.time()
# 
# 
# tally = read_rds("data.rds") %>%
#   group_by(year, breed)   %>%
#   summarize(n = n()) %>%
#   mutate(text = paste(
#     "<b>", year, "</b>",
#     "<br>",
#     "<i>", breed, "</i>", n))
# 
# gg = ggplot() +
#   geom_line(
#     data = tally,
#     mapping = aes(
#       x = year, y = n,
#       group = breed, color = breed,
#       text = text)) 
# 
# ggplotly(gg, tooltip = "text")







data %>%
  group_by(breed) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  filter(breed %in% c("BEAGLE", "CHIHUAHUA", "WELSH CORGI PEMBROK",
                      "WELSH CORGI CARDIGA", "MIXED"))
arrange(desc(n))

data %>%
  group_by(breed, zip) %>%
  summarize(n = n()) %>%
  ungroup()



data %>% 
  filter(breed == "")

# data %>%
#   group_by(breed, zip) %>%
#   mutate(n = n()) %>%
#   ungroup



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



