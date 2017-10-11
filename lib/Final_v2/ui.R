packages.used <- c("shinydashboard", "ggmap", "ggplot2", "leaflet", 
                   "htmltools", "dplyr", "plyr", "httr", 
                   "reshape2", "purrr",
                   "shinycssloaders", "gepaf", 
                   "stringr", "geosphere", "DT", "shinyjs", "markdown")

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
library(shinyjs)
library(DT)
library(markdown)


dashboardPage(
  
  skin = "purple",
  dashboardHeader(
    title = 'Interesting Places on Your Way',
    titleWidth = 320
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(id='sidebarmenu',
                menuItem("Map",tabName="locator",icon=icon("map")),
                menuItem("List of Businesses",tabName="rawdata",icon=icon("book")),
                menuItem("About",tabName="home",icon=icon("user-circle"))
    ),
    textInput("from","Enter Point A:",'Times Square, New York'),
    textInput("to","Enter Point B:",'Columbia University, New York'),
    numericInput("in_mile","Max Dist From Your Route (mi)",0.2, min = 0.1, max = 5),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1, step = 0.5),
    sliderInput("maxPrice","Price Range",min = 1, max = 4, value = 4, step = 1),
    div(style="display:inline-block;width:80%;text-align: center;",
        submitButton("Submit / Refresh"))
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
    tabItems(
      tabItem(tabName='locator',
              fluidRow(
                column(width = 12,box(width = NULL, solidHeader = TRUE,
                                      withSpinner(leafletOutput("map"))))),
              fluidRow(box(width = 12, solidHeader = TRUE, 
                           htmlOutput("transit_information"))),
              fluidRow(box(width = 8,
                           htmlOutput("Click_review_text"),
                           htmlOutput("Click_review_rating"),
                           htmlOutput("Click_review_time")), box(width = 4, uiOutput("image")))),
      tabItem(tabName = 'rawdata',
              dataTableOutput("mytable")
              
      ),
      
      tabItem(tabName='home', 
              fluidRow(
                column(width=12,
                       collapsible=T,
<<<<<<< HEAD:lib/Final_v2/ui.R
                       includeMarkdown('overview.md')
=======
                       #htmlOutput("Overview")
                       includeMarkdown("overview.md")
>>>>>>> e8e0253fca7b8b7021ada25c2b7baf8f371cdfb0:lib/New/Final_v2/ui.R
                )
              )
      )
    )
    ))
