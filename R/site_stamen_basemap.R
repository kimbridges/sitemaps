#' Create a custom basemap using Stamen Maps as the server.
#'
#' @description
#' A basemap is a key element in making a map. The Stamen Map server provides
#' so useful designs, particularly for maps that show country-size areas or
#' smaller.
#'
#' @param datatable The table with the text, location and (optionally) style data
#' @param styles A dataframe with style values for some map characteristics
#'
#' @return A basemap that is scaled to hold the data points
#' @import ggmap
#' @export
#'
site_stamen_basemap <- function(datatable, styles = column){

  ## Get a bounding box
  bb <- make_bbox(lon=datatable$lon, lat = datatable$lat, f=styles$margin)

  ## Make bounding box square, if requested
  if(styles$square){
    bb <- site_squaremap(bb)
  }

  ## Get the Stamen Map
  basemap <- get_stamenmap(bbox = c(left=bb[[1]], 
                                    bottom=bb[[2]], 
                                    right=bb[[3]], 
                                    top=bb[[4]]),
                           maptype = styles$smaptype)

  return(basemap)
}
