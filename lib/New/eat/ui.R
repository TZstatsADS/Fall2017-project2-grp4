#using 'shinydashboard' library

library(shinydashboard)
dbHeader<-dashboardHeader(title='Restaurants on your way', titleWidth = 350)

dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = 'Restaurants on your way',
    titleWidth = 310
  ),
  
  dashboardSidebar(
    width = 310,
    textInput("from","Enter starting point:",'Time Square,NYC, NY, USA'),
    textInput("to","Enter destination:",'Columbia University, NYC, NY, USA'),
    numericInput("in_mile","Max dist from your route (mi)",0.2, min = 0.1, max = 5),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1),
    sliderInput("price","Price Range",min = 1, max = 4, value = 1),
    submitButton("Submit",width='100%')
  ),
  
  dashboardBody(
    fluidRow(
      column(width = 7,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("Map")))),
    fluidRow(
      column(width=7,
             box(width = NULL, solidHeader = TRUE,
                 verbatimTextOutput("Click_text")),
                 htmlOutput("image"))
   )
  )
)











