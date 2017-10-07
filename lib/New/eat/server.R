#FOR OUR GROUP

options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {
  
  
  #generate data through input: result[1] = route, result[2] = ctb(to be debugged if only starting point)
  Data<-reactive({
    result<-list()
    
    
    if(!is.null(input$from) & !is.null(input$to)){
      route_df <- route(input$startingPoint, input$destination, structure = 'route', mode = 'transit',output = "simple")# transit
      
      for (i in 1:dim(route_df)[1]){
        route_df$address[i] = revgeocode(c(as.numeric(route_df$lon[i]),as.numeric(route_df$lat[i])))
        route_df$url[i]=paste0("https://www.yelp.com/search?f&find_loc=",gsub('\\s',"+",  route_df$address[i]),"&ns=1",sep="")
        route_df$content[i] = paste(sep = "<br/>",
                                    "<b><a href=route_df$url[i]</a></b>",
                                    route_df$address[i])
      }
      result[[1]] <- route_df
    }
    