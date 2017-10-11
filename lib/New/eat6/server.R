#FOR OUR GROUP

options(shiny.maxRequestSize=50*1024^2)

shinyServer(function(input, output, session) {
  

  #generate data through input: result[1] = route, result[2] = ctb(to be debugged if only starting point)
    Data<-reactive({
      result<-list()
    
       
      if(!is.null(input$startingPoint) & !is.null(input$destination)){
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
      
      
      #TO BE DEBUGGED: ONLY STARTING POINT SITUATION
      
      '''
      else if (!is.null(input$startingPoint)){ 
      route_df <- route(input$startingPoint, structure = 'route', mode = 'transit',output = "simple")# transit
      #structure = "route"?? and map code need to be changed
      
      for (i in 1:dim(route_df)[1]){
      route_df$address[i] = revgeocode(c(as.numeric(route_df$lon[i]),as.numeric(route_df$lat[i])))
      route_df$url[i]=paste0("https://www.yelp.com/search?f&find_loc=",gsub('\\s',"+",  route_df$address[i]),"&ns=1",sep="")
      route_df$content[i] = paste(sep = "<br/>",
      "<b><a href=route_df$url[i]</a></b>",
      route_df$address[i])
      
      }
      
      result[[1]] <- route_df
      
    }
      '''
      
      
      
      #use yelp api we need to change it because for some reasons spa and salesman come in. remove them out
      res = POST("https://api.yelp.com/oauth2/token",
                 body = list(grant_type = "client_credentials",
                             client_id = "KjjqVxt9pcpH6fWmEqVvEQ",
                             client_secret = "ApFCYwJTIteL7mu146tmmLMuCui7unn2dcKN3ScFTrSLRLm8QWKNqkTWXtHz4OY1"))
      token = content(res)$access_token
      yelp = "https://api.yelp.com"
      
      #create an empty ctb dataframe
      ctb = data.frame(matrix(NA, nrow = 0, ncol = 13))
      
      #a for loop for 4 locations in this case 
      for (i in 1:nrow(route_df)){
        url_i =modify_url(yelp, path = c("v3", "businesses", "search"),
                          query = list(latitude =route_df$lat[i],longitude = route_df$lon[i],
                                       open_now = T,
                                       radius =round(1609*in_mile,0)))
        res_i = GET(url_i, add_headers('Authorization' = paste("bearer", token)))
        #http_status(res)
        ctb_list_i = content(res_i)$businesses
        ctb_i=data.frame(matrix(NA, nrow = 0, ncol = 13))
        for (j in 1:length(ctb_list_i)){ # create a J row dataframe
          ctb_i[j,1] = ctb_list_i[[j]]$id
          ctb_i[j,2] = ctb_list_i[[j]]$name
          ctb_i[j,3] = ctb_list_i[[j]]$image_url
          ctb_i[j,4] = ctb_list_i[[j]]$url
          ctb_i[j,5] = ctb_list_i[[j]]$review_count
          ctb_i[j,6] = ctb_list_i[[j]]$rating
          ctb_i[j,7] = ifelse (is.null(ctb_list_i[[j]]$price),NA,ctb_list_i[[j]]$price)
          ctb_i[j,8] = ctb_list_i[[j]]$display_phone
          ctb_i[j,9] = ctb_list_i[[j]]$coordinates$latitude
          ctb_i[j,10] = ctb_list_i[[j]]$coordinates$longitude
          ctb_i[j,11] = ctb_list_i[[j]]$location$display_address[[1]]
          ctb_i[j,12] = ctb_list_i[[j]]$location$zip_code
          ctb_i[j,13] = i 
        }
        ctb=rbind(ctb, ctb_i)
      }
      
      colnames(ctb) = c("id","name","image_url","url","review_count","rating","price","display_phone","latitude","longitude","address","zip code","stop")
      
      ctb$price = factor(ctb$price)
      
      result[[2]]<-ctb[
        ctb$review_count>=input$minReview&
          ctb$rating>=input$minStar,]
        
      #browser()
      return(result)
    })
    
    
    
    #TO BE DEBUGGED
    
    
    
  
    
    output$Map <- renderLeaflet({
      #browser()
      ctb <- Data[[1]]
      route_df <- Data[[2]]
      factpal = colorFactor(rainbow(5), ctb$price)
      leaflet(route_df) %>% addTiles() %>%
        addMarkers(~lon, ~lat, popup = ~htmlEscape(address))%>%
        addCircles(lng = ~lon, lat = ~lat, weight = 1,radius =1609*in_mile)%>%
        addPolylines(~lon, ~lat,color="red")%>%
        addCircleMarkers(ctb$longitude, ctb$latitude, radius = ctb$rating, stroke = FALSE, 
                         fillOpacity = ((ctb$review_count - min(ctb$review_count)) / max(ctb$review_count - min(ctb$review_count)))+0.4,
                         color = factpal(ctb$price), 
                         popup = paste( paste0("<b><a href=", "'",ctb$url,"'>", ctb$name, "</a></b>"),"<br>",
                                        "Address :",ctb$address,"<br>",
                                        paste0("Phone: ", "<b><a href=tel:", "'",ctb$display_phone,"'>", ctb$display_phone, "</a></b>"),"<br>",
                                        "Rating:", ctb$rating, "<br>"
                         ))%>%
        addLegend(pal = factpal, values = ctb$price,
                  title = "Price Range",
                  opacity = 1
        )
        
        
           })
    

    
    observeEvent(input$map_marker_click, {
      #browser()
      click <- input$map_marker_click
      if(!is.null(click$id)){
        if(is.null(input$id)) updateSelectInput(session, "id", selected=click$id)
        if(!is.null(input$id) && input$id!=click$id) updateSelectInput(session,"id",selected=click$id)
      }
    })
    
    #TO BE ADDED SUPERLINK TO YELP 
    
    output$clickedName<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        return(as.character(ctb$name[ctb$id == input$map_marker_click$id]))
      }
    })
    
    #TO BE ADDED SUPERLINK TO YELP 
    
    
    output$clickedNameAddress<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        return(paste0('Address: ',ctb$address[ctb$id == input$map_marker_click$id]))
      }
    })
    
    
    
    
    output$clickedNameRating<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        return(paste0(ctb$rating[ctb$id == input$map_marker_click$id],
                      ' stars out of 5 on Yelp'))
      }
    })
    
    output$clickedNameReviewCount<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        return(paste0(ctb$review_count[ctb$id == input$map_marker_click$id],
                      'number of reveiews on Yelp'))
      }
    })
    
    output$clickedNamePriceRange<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        return(paste0('Price range: ', ctb$price[ctb$id == input$map_marker_click$id]))
      }
    })
    
    output$clickedNamePhone<-renderText({
      #browser()
      if(is.null(input$map_marker_click))
        return(NULL)
      else{
        ctb <- Data()[[2]]
        if(nchar(as.character(ctb$display_phone[ctb$id == input$map_marker_click$id]))==10){
          phone <- ctb$display_phone[ctb$id == input$map_marker_click$id]
          return(paste0('Phone #: (',substr(phone,1,3),')',substr(phone,4,6),'-',substr(phone,7,10)))
        }
        else
          return(paste0('Phone #: ',ctb$display_phone[ctb$id == input$map_marker_click$id]))
      }
    })


  })