#using 'shinydashboard' library

packages.used <- c("shinydashboard", "ggmap", "ggplot2", "leaflet", 
                   "htmltools", "dplyr", "plyr", "httr", 
                   "reshape2", "purrr",
                   "shinycssloaders", "gepaf", 
                   "stringr", "geosphere")

#check packages that need to be installed

packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))

#install additional packages

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shinydashboard)
library(ggmap)
library(ggplot2)
library(leaflet)
library(htmltools)
library(dplyr)
library(plyr)
library(httr)
library(reshape2)
library(purrr)
library(shinycssloaders)
library(gepaf)
library(stringr)
library(geosphere)




dashboardPage(
  skin = "black",
  dashboardHeader(
    title = 'Interesting Places on Your Way',
    titleWidth = 310
  ),
  
  dashboardSidebar(
    width = 310,
    textInput("from","Enter starting point:",'Times Square, New York'),
    textInput("to","Enter destination:",'Columbia University, New York'),
    numericInput("in_mile","Max dist from your route (mi)",0.2, min = 0.1, max = 5),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1, step = 0.5),
    sliderInput("maxPrice","Price Range",min = 1, max = 4, value = 4, step = 1),
    div(style="display:inline-block;width:80%;text-align: center;",
        submitButton("Submit / Refresh"))
    #fluidRow(
    #  column(width = 6,
    #    dropdownButton(label = "Select Cuisines", status = "default", width = 80,
    #      checkboxGroupInput(inputId = "check", label = "Choose", choices = 1:20)
    #    ),
    #    verbatimTextOutput(outputId = "cuisine")))
  ),
  
  dashboardBody(
    includeCSS('./www/bootstrap.min.css'),
    tags$head(tags$style(HTML('
      .main-header .logo {
                              
                              font-family: Menlo, Monaco, Consolas, "Courier New", monospace;
                              
                              font-weight: bold;
                              font-size: 16px;
                              }
                              '))),
    fluidRow(
      column(width = 12,
             box(width = NULL, solidHeader = TRUE,
                 withSpinner(leafletOutput("map"))))),
    fluidRow(box(width = 8,
                 htmlOutput("Click_review_text"),
                 htmlOutput("Click_review_rating"),
                 htmlOutput("Click_review_time")), box(width = 4, uiOutput("image")))
  )
)












