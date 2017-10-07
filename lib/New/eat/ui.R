#using 'shinydashboard' library

#FOR OUR GROUP

#dbHeader<-dashboardHeader(title='Restaurants on your way', titleWidth = 350)

dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(
    title = 'Restaurants on your way',
    titleWidth = 310
  ),
  
  dashboardSidebar(
    width = 310,
    sidebarMenu(id='sidebarmenu', #bookmark
                #sidebarMenu(id='sidebar',
                #menuItem("Cuisine Overview",tabName="overview",icon=icon("pie-chart")),
                menuItem("Restaurants",tabName="locator",icon=icon("cutlery")),
                menuItem("github", href = "https://github.com/rstudio/shinydashboard/", 
                         icon = icon("file-code-o")) # to be add a url
                
    ),
    
    textInput("startingPoint","Enter starting point:",'Time Square,NYC, NY, USA'),
    textInput("destination","Enter destination:",'Columbia University, NYC, NY, USA'),
    sliderInput("distance","Max dist from your route (mi)",
                min = 1, max = 21, value = 1),
    
    selectInput("cuisine","Cuisine:",levels(uniqueRestau5$Cuisine),'Ice Cream',multiple=T),
    sliderInput("minReview","Minimum # of reviews on Yelp",min = 1, max = 100, value = 1),
    sliderInput("minStar","Minimum # of stars on Yelp",min = 1, max = 5, value = 1),
    sliderInput("minSafetyScore","Minimum safety score",min = 0, max = 1, value = 0),
    submitButton("Submit",width='100%')
    
    
  ),
  
  dashboardBody(
    includeCSS('./www/custom.css'),
    tabItems(
      
      tabItem(tabName = "overview",
              fluidRow(
                column(width=12,
                       box(width=NULL,
                           title=tagList(shiny::icon("pie-chart"),"Cuisine distribution at your location"),
                           status='primary',
                           collapsible=T,
                           showOutput("pieChart","highcharts") # TO BE DEBUGGED
                       )))),
      
      tabItem(tabName='locator',
              fluidRow(
                column(width = 7,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("Map"))),
                column(width=5,
                       box(title = "Selected Restaurant", status = "primary",
                           width=NULL,solidHeader=T,
                           textOutput("clickedName"),
                           br(),
                           uiOutput('image'),
                           br(),
                           textOutput("clickedNameAddress"),
                           textOutput("clickedNameGrade"),
                           textOutput("clickedNameCritical"),
                           textOutput("clickedNameRating"),
                           textOutput("clickedNameReviewCount"),
                           textOutput("clickedNamePriceRange"),
                           textOutput("clickedNamePhone")))),
              fluidRow(column(width=5,
                              selectInput("nameId","Restaurant Id",c("",sort(uniqueRestau5$NameId)),
                                          selected="",multiple=F,width="100%")))
      )
    )
    
  )
)

