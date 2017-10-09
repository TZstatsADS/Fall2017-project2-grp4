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
library(shinycssloaders)

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}


dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = 'Interesting Places on Your Way',
    titleWidth = 310
  ),
  
  dashboardSidebar(
    width = 310,
<<<<<<< HEAD
    textInput("from","Enter a Starting Point:",'Time Square,NYC, NY, USA'),
    textInput("to","Enter a Destination:",'Columbus Circle, NYC, NY, USA'),
    numericInput("in_mile","Max Dist From Your Route (mi)",0.2, min = 0.1, max = 5),
    sliderInput("minStar","Minimum # of Stars on Yelp",min = 1, max = 5, value = 1),
=======
    textInput("from","Enter starting point:",'Time Square,NYC, NY, USA'),
    textInput("to","Enter destination:",'Columbus Circle, NYC, NY, USA'),
    numericInput("in_mile","Max dist from your route (mi)",0.2, min = 0.1, max = 5),
    div(style="display:inline-block;width:80%;text-align: center;",submitButton("Submit")),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1),
>>>>>>> 419435136fe8a9b3a09d5eccd51e5c407ede4bb4
    sliderInput("price","Price Range",min = 1, max = 4, value = 1),
    submitButton("Submit / Refresh",width='60%'),
    fluidRow(
      column(width = 6,
        dropdownButton(label = "Select Cuisines", status = "default", width = 80,
          checkboxGroupInput(inputId = "check", label = "Choose", choices = 1:20)
        ),
        verbatimTextOutput(outputId = "cuisine")))
  ),
  
  dashboardBody(
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












