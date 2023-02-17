# Homework #2 -- Building an R Shiny Dashboard

## Live Link

You can view the live app [here](https://mgermaine93.shinyapps.io/mgermai2-HW2/).

## General Description

This is my second of three homework assignments for CMU's Spring 2023 "R for Operations Management" course.

## Assignment Details and Directions

Creating multiple types of visuals from the same data is an important way to convey information to application users. Students will create a Dashboard using a static download of an Open Data or a Dataset from their own place of employment (make sure you have permission to use it for this assignment first!)

Students may make their application in either flexdashboard or shinydashboard layouts. The final dashboard should be working and deployed onto shinyapps.io.

### Directions:

Include at least:
* Three (3) input/filters
* Three (3) single numeric based boxes/gauges
* One (1) datatable
* Three (3) interactive and reactively responsive charts.
* These elements should be placed throughout a dashboard with at least three (3) pages or tabs with analytical themes or questions about the data.
* On the server side your plots and tables must utilize the reactive function for any and all datasets.
* Your final app must work when deployed to shinyapps.io.

## Data Source

The data source I used is once again courtesy of the [Western Pennsylvania Regional Data Center](http://www.wprdc.org/).

The specific dataset I used (along with its dictionary) is [here](https://data.wprdc.org/dataset/allegheny-county-dog-licenses).

Since combining all of the files proved to be too large for github, I cleaned the data so that `dog_licenses.csv` only contains data from the years 2012 through 2022 of 20 selected dog breeds.

## Improvements I Would Make in the Future

Given more time, there are a few improvements I would make:

* For the `Top Dog Breeds by Zip Code` plot, I would dynamically make new plots for each zip code that the user selected.  For example, I the single plot that currently displays is data from a single user-input zip code.  This improvement would make it so that a user could select multiple zip codes, and a plot would dynamically generate on the page for each zip code, up to a certain amount.
* I would implement the [scales](https://scales.r-lib.org/) package, particularly on the x-axes that involve years.
* I would implement dynamic numeric-based boxes/gauges, rather than the static ones that are currently in the app.