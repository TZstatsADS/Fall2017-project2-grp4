#using 'shinydashboard' library

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


dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = 'Restaurants on your way',
    titleWidth = 310
  ),
  
  dashboardSidebar(
    width = 310,
    textInput("from","Enter starting point:",'Time Square,NYC, NY, USA'),
    textInput("to","Enter destination:",'Columbus Circle, NYC, NY, USA'),
    numericInput("in_mile","Max dist from your route (mi)",0.2, min = 0.1, max = 5),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1),
    sliderInput("price","Price Range",min = 1, max = 4, value = 1),
    submitButton("Submit",width='100%')
  ),
  
  dashboardBody(
    fluidRow(
      column(width = 12,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("Map")))),
    fluidRow(
      column(width=12,
             box(width = NULL, solidHeader = TRUE,
                 verbatimTextOutput("Click_review_text"),
                 verbatimTextOutput("Click_review_rating"),
                 verbatimTextOutput("Click_review_time"),
                 uiOutput("image"))))
    #fluidRow(
      #column(width = 12, box(width = NULL,  fluidRow(htmlOutput("picture"))))
    )
)










