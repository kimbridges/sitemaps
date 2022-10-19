#' Return the elevations for a datatable with geographic coordinates.
#'
#' @description
#' This uses Google Maps to get the elevations of locations in a datatable.
#' 
#' The process involves an API call to Google Maps. As a result, you need
#' to have a valid Google Maps Key. This key is sent to Google Maps each time
#' you ask for an elevation. The explicit use of the key in this function
#' makes it a bit different than the other sitemap functions that use the Google
#' Maps server.
#'
#' @param datatable The standard datatable with columns for lat, lon
#' @param APIkey A Google Maps API key
#'
#' @return datatable with elevations in a column named 'elevation'
#' @import httr jsonlite
#' @export
#'
site_elevation <- function(datatable, APIkey){
  
  ## Establish the Google Map API URL
  g_base <- "https://maps.googleapis.com/maps/api/elevation/json?"
  
  ## Get the number of table rows
  nrows <- nrow(datatable)  
  
  ## Loop through the table
  for (i in 1:nrows){
    g_locs   <- paste0("locations=",datatable$lat[[i]],",",
                     datatable$lon[[i]])
    g_string <- paste0(g_base,g_locs,"&key=",APIkey)
    
    ## Send the request
    g_return <- GET(g_string)
    g_data   <- content(g_return,"text")
    
    elev_data <- fromJSON(g_data)  
    elev_results <- elev_data$results
    datatable$elevation[[i]] <- as.integer(elev_results$elevation)
    
    ## Data returned in the API call
    ## elev_results$elevation
    ## elev_results$location
    ## elev_results$resolution
    
  } ## end of loop
  
  ## unlist the elevations
  datatable$elevation <- unlist(datatable$elevation)
  
  return(datatable)
  
} 