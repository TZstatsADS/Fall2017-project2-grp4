#FOR OUR GROUP

shinyServer = function(input, output, session) {
  
########## route_df new 
  route_df = reactive({
    route_dfa = route(input$from, input$to, structure = 'route', mode = 'transit',output = "all")
    latlon=decodePolyline(enc_polyline =route_dfa$routes[[1]]$overview_polyline$points)
    steps = length(route_dfa$routes[[1]]$legs[[1]]$steps)
    route_df = data.frame(matrix(nrow = (steps+1))) # define a new matrix and this matrix will replace route_df
    
    # adding instruction, lats, lngs, address, content to each step
    for (i in 1:steps){
      route_df$lat[i] = route_dfa$route[[1]]$legs[[1]]$steps[[i]]$start_location$lat
      route_df$lon[i] = route_dfa$route[[1]]$legs[[1]]$steps[[i]]$start_location$lng
      route_df$address[i] = revgeocode(c(as.numeric(route_df$lon[i]),as.numeric(route_df$lat[i])))
      route_df$url[i]=paste0("https://www.yelp.com/search?f&find_loc=",gsub('\\s',"+",  route_df$address[i]),"&ns=1",sep="")
      route_df$content[i] = paste("<b><a href=","'",route_df$url[i],"'>",route_df$address[i],"</a></b>","<br>")
      route_df$instruction[i] = route_dfa$route[[1]]$legs[[1]]$steps[[i]]$html_instructions
    }
    
    # the end location is different than others
    route_df$lat[steps+1] = route_dfa$route[[1]]$legs[[1]]$steps[[steps]]$end_location$lat
    route_df$lon[steps+1] = route_dfa$route[[1]]$legs[[1]]$steps[[i]]$end_location$lng
    route_df$address[steps+1] = revgeocode(c(as.numeric(route_df$lon[steps]),as.numeric(route_df$lat[steps])))
    route_df$url[steps+1]=paste0("https://www.yelp.com/search?f&find_loc=",gsub('\\s',"+",  route_df$address[steps]),"&ns=1",sep="")
    route_df$content[steps+1] = paste("<b><a href=","'",route_df$url[steps],"'>",route_df$address[steps],"</a></b>","<br>")
    route_df$instruction[steps+1] = "Reach Destination"
    
    trainsteps = c()
    j = 1
    for (i in 1:steps){
      if (!is.null(route_dfa$routes[[1]]$legs[[1]]$steps[[i]]$transit_details)){
        if(route_dfa$routes[[1]]$legs[[1]]$steps[[i]]$transit_details$line$vehicle$name == 'Subway'){
          trainsteps <- c(trainsteps, i)
          j = j+1
        }
      }
    }
    
    for (i in trainsteps){
      route_df$instruction[i] <- paste(route_dfa$routes[[1]]$legs[[1]]$steps[[i]]$transit_details$line$short_name, route_df$instruction[i] )
    }
    return(list(route_df,latlon))
  })
 

  
  
  
  
  #####Create ctb dataframe that contains all the restaurant information 
  ctb <- reactive({
    res = POST("https://api.yelp.com/oauth2/token",
               body = list(grant_type = "client_credentials",
                           client_id = "KjjqVxt9pcpH6fWmEqVvEQ",
                           client_secret = "ApFCYwJTIteL7mu146tmmLMuCui7unn2dcKN3ScFTrSLRLm8QWKNqkTWXtHz4OY1"))
    token = content(res)$access_token
    yelp = "https://api.yelp.com"
    
    # create an empty ctb dataframe
    ctb = data.frame(matrix(NA, nrow = 0, ncol = 14))
    
    # a for loop for 4 locations in this case 
    for (i in 1:nrow(route_df()[[1]])){
      url_i =modify_url(yelp, path = c("v3", "businesses", "search"),
                        query = list(latitude =route_df()[[1]][i,'lat'],longitude = route_df()[[1]][i,'lon'],
                                     open_now = T,
                                     radius =round(1609* input$in_mile ,0)))
      res_i = GET(url_i, add_headers('Authorization' = paste("bearer", token)))
      #http_status(res)
      ctb_list_i = content(res_i)$businesses
      ctb_i=data.frame(matrix(NA, nrow = 0, ncol = 14))
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
        ctb_i[j,13] = ctb_list_i[[j]]$categories[[1]]$title
        ctb_i[j,14] = i 
        
      }
      ctb=rbind(ctb, ctb_i)
    }
    
    colnames(ctb) = c("id","name","image_url","url","review_count","rating","price","display_phone","latitude","longitude","address","zip code","category","stop")
    
    ctb$price = factor(ctb$price)
    
    for (i in 1:dim(ctb)[1]){
      url_rev_i = paste0("https://api.yelp.com/v3/businesses/",ctb$id[i],"/reviews")
      res_rev_i=GET(url_rev_i, add_headers('Authorization' = paste("bearer", token)))
      ctb$review_time[i] = content(res_rev_i)$review[[1]]$time_created # for review created time need to change
      ctb$review_rating[i] = content(res_rev_i)$review[[1]]$rating
      ctb$review_text[i] = content(res_rev_i)$review[[1]]$text
    }
    
    return(ctb)
  })
  
  #####Create Palette Variable
  color = function(){
    return(colorFactor(rainbow(5), ctb()$price))
  }
  
  #####Create a complete route dataframe
  route_df2 = reactive({
    return(cbind(route_df()[[1]],mean_stop =round(tapply(ctb()$rating,ctb()$stop,mean),2)))
  })
  
  
  #####Create map
  
  #data_of_click <- reactiveValues(clickedMarker=NULL)
    
  output$map = renderLeaflet({
     leaflet() %>% addTiles() %>%
      addMarkers(route_df2()$lon, route_df2()$lat, popup = paste(route_df2()$content,"<br>",
                                                                 "Overall Rating: ", "<b>",route_df2()$mean_stop,"</b>"))%>%
      addCircles(lng = route_df()[[1]]$lon, lat = route_df()[[1]]$lat, weight = 1,radius =1609* input$in_mile)%>%
      addPolylines(lng = route_df()[[2]]$lon, lat = route_df()[[2]]$lat,color="red")%>%
      addCircleMarkers(data = {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)},
                       lng = ctb()$longitude, lat = ctb()$latitude, radius = ctb()$rating+1, stroke = FALSE, # add 1 to make points bigger
                       fillOpacity = ((ctb()$review_count - min(ctb()$review_count)) / max(ctb()$review_count - min(ctb()$review_count)))+0.4,
                       color = ~{color = colorFactor(rainbow(5), ctb()$price)
                       color({ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$price)},
                       layerId = {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$id,
                       popup = paste("<b><a href=", "'",{ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$url,"'>", {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$name, "</a></b>","<br>",
                                     "Address: ",{ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$address ,"<br>",
                                     "Phone: ", "<a href=tel:", "'",{ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$display_phone,"'>", {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$display_phone, "</a>","<br>",
                                     "Rating: ", {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$rating, "<br>"
                      ))%>%
      addLegend(pal = colorFactor(rainbow(5), ctb()$price), values = {ctb() %>% filter(rating >= input$minStar & as.numeric(ctb()$price) <= input$maxPrice)}$price,
                title = "Price Range",
                opacity = 1
      ) 
  })
  
  

  
  observe({
    click = input$map_marker_click
    if(is.null(click))
      return()
    review_text = paste('<span style="color:#5F5DA3"> The most current review: </span><br>',"<b>",ctb()$review_text[ctb()$id==click$id],"</b>")
    review_rating = paste('<span style="color:#5F5DA3"> Rating of this Reviewer: </span><br>',"<b>",ctb()$review_rating[ctb()$id==click$id],"</b>")
    review_time = paste('<span style="color:#5F5DA3"> The Rating Time: </span><br>',"<b>",ctb()$review_time[ctb()$id==click$id],"</b>")
    

    output$Click_review_text<-renderText({review_text})
    output$Click_review_rating<-renderText({review_rating})
    output$Click_review_time<-renderText({review_time})
    
    output$image<-renderUI({
      click<-input$map_marker_click
      if(is.null(click))
        return(NULL)
      else{
        src<-ctb()$image_url[ctb()$id==click$id]
        return(tags$img(src=src, width = '200px', height='200px'))
      }
    })
  })
}
    
#output$text1 <- renderText({ paste("hello input is","<font color=\"#FF0000\"><b>", input$n, "</b></font>") })

#paste('<span class="red">',"The most current review:</span>","<br>",ctb()$review_text[ctb()$id==click$id])



