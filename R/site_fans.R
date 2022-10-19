#' Create map symbols that show a direction and field of view.
#'
#' @description
#' Photo locations on a map may be more useful if the direction (bearing) 
#' of the photo and its field of view are part of the symbol marking the
#' place where the photo was taken. This is called a 'fan.'
#' 
#' The minimal data, beyond the normal data point info (i.e., lon, lat),
#' is the fan_direction and fan_field_width (i.e., the field of view). 
#' Each of these specifications is either in
#' a column in a data table or, if the value is constant, a column
#' style value (e.g., column$field_width <- 48).
#' 
#' There are a number of styles that go with the site_fans function. The
#' default values are loaded with site_styles.
#'
#' @param datatable The standard datatable with columns for lat, lon,
#' fan_direction and fan_field_width
#' @param styles The standard complement of column values
#'
#' @return Data supplied to ggmap that places a layer with symbols on the basemap
#' @import tibble stringr ggplot2 geosphere
#' @export
#'
site_fans <- function(datatable, styles = column){
  
  ## Bind some variables
  . <- lon <- lat <- arm_left_lon <- arm_left_lat <- NULL
  arm_right_lon <- arm_right_lat <- x_set <- y_set <- set <- fill <- NULL
  
  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    add_column(!!!styles[!names(styles) %in% names(.)])  
  
  ## Needs the following info
  ## datatable$lon
  ## datatable$lat
  ## datatable$text
  ## datatable$fan_direction
  ## datatable$fan_field_width
  ## datatable$fan_arm_length
  ## datatable$fan_arm_thickness
  ## datatable$fan_arm_color
  ## datatable$fan_arm_linetype
  ## datatable$fan_color
  ## datatable$fan_alpha
  ## styles$fan_fill
  
  ## Calculate the arm angles
  datatable$arm_left <-datatable$fan_direction-(datatable$fan_field_width)/2 
  datatable$arm_right<-datatable$fan_direction+(datatable$fan_field_width)/2 
  datatable$arm      <- datatable$fan_arm_length 
  
  ## Determine how many symbols are needed
  rows <- nrow(datatable)
  
  datatable$arm_left_lon   <- NULL
  datatable$arm_left_lat   <- NULL
  datatable$arm_right_lon  <- NULL
  datatable$arm_right_lat  <- NULL
  datatable$arm_center_lon <- NULL
  datatable$arm_center_lat <- NULL
  
  ## Get the endpoints for the arms (center, left, right)
  for(i in 1:rows){
    center_end <- data.frame(destPoint(c(datatable$lon[[i]],
                                         datatable$lat[[i]]),
                                       datatable$fan_direction[[i]],
                                       datatable$arm[[i]]))
    
    datatable$arm_center_lon[[i]] <- center_end$lon
    datatable$arm_center_lat[[i]] <- center_end$lat
    
    left_end <- data.frame(destPoint(c(datatable$lon[[i]],
                                       datatable$lat[[i]]),
                                     datatable$arm_left[[i]],
                                     datatable$arm[[i]]))
    
    datatable$arm_left_lon[[i]] <- left_end$lon
    datatable$arm_left_lat[[i]] <- left_end$lat
    
    right_end <- data.frame(destPoint(c(datatable$lon[[i]],
                                        datatable$lat[[i]]),
                                      datatable$arm_right[[i]],
                                      datatable$arm[[i]]))
    
    datatable$arm_right_lon[[i]] <- right_end$lon
    datatable$arm_right_lat[[i]] <- right_end$lat
    
  } ## end loop for arm endpoints
  
  ## Unlist the values for the arm endpoints
  datatable$arm_center_lon <-
    unlist(datatable$arm_center_lon)
  datatable$arm_center_lat <-
    unlist(datatable$arm_center_lat)
  
  datatable$arm_left_lon <-
    unlist(datatable$arm_left_lon)
  datatable$arm_left_lat <-
    unlist(datatable$arm_left_lat)
  
  datatable$arm_right_lon <-
    unlist(datatable$arm_right_lon)
  datatable$arm_right_lat <-
    unlist(datatable$arm_right_lat)
  
  ## Put on both data points and arms
  
  arm_left <- geom_segment(data = datatable,
                           aes(x=lon,
                               y=lat,
                               xend=arm_left_lon,
                               yend=arm_left_lat),
                           size=datatable$fan_arm_thickness,
                           linetype=datatable$fan_arm_linetype,
                           color=datatable$fan_arm_color) 
  
  arm_right <- geom_segment(data = datatable,
                            aes(x=lon,
                                y=lat,
                                xend=arm_right_lon,
                                yend=arm_right_lat),
                            size=datatable$fan_arm_thickness,
                            linetype=datatable$fan_arm_linetype,
                            color=datatable$fan_arm_color) 
  
  if(isFALSE(styles$fan_fill)){
    return(c(arm_left, arm_right))} else {
      
      ## Ready the data to fill the area between the arms
      poly_data_left <- data.frame(
        x_set=c(datatable$lon, 
                datatable$arm_left_lon, 
                datatable$arm_center_lon),
        y_set=c(datatable$lat, 
                datatable$arm_left_lat, 
                datatable$arm_center_lat),
        set=datatable$text,
        fill=datatable$fan_color,
        alpha=datatable$fan_alpha)
      
      poly_data_right <- data.frame(
        x_set=c(datatable$lon, 
                datatable$arm_right_lon, 
                datatable$arm_center_lon),
        y_set=c(datatable$lat, 
                datatable$arm_right_lat, 
                datatable$arm_center_lat),
        set=datatable$text,
        fill=datatable$fan_color,
        alpha=datatable$fan_alpha)
      
      ## Fill in the fan areas & label the points
      fill_left <-  geom_polygon(data = poly_data_left, 
                                 aes(x=x_set, y=y_set,group=set,
                                     fill=fill,alpha=alpha)) 
      # fill = datatable$fan_color,
      # alpha=datatable$fan_alpha)
      fill_right <- geom_polygon(data = poly_data_right, 
                                 aes(x=x_set, y=y_set,group=set,
                                     fill=fill,alpha=alpha)) 
      # fill = datatable$fan_color,
      # alpha=datatable$fan_alpha)
      
      ## Return the fan components
      return(c(arm_left, arm_right, fill_left, fill_right))
    } ## end else
  
} ## end function site_fans

site_fans
