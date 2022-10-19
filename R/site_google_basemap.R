#' Create a custom basemap using Google Maps as the server.
#'
#' @description
#' A basemap is a key element in making a map. The Google Map server provides
#' several good choices that show different types of information (e.g., terrain)
#' on the basemap.
#'
#' @param datatable The table with the location coordinates (as lat and lon)
#' @param styles A dataframe with style values for each of the map characteristics
#'
#' @return A basemap that is scaled to hold the data points
#' @import ggmap
#' @export
#'
site_google_basemap <- function(datatable, styles = column){

  ## Get a bounding box
  bb <- make_bbox(lon=datatable$lon, lat = datatable$lat, f=styles$margin)

  ## Use the bounding box to get the center and zoom for a Google map
  center_zoom <- bb2google(bb=bb)

  ## Get the Google Map
  basemap <- get_googlemap(center  = center_zoom$center,
                           zoom    = center_zoom$zoom,
                           maptype = styles$gmaptype,
                           scale   = styles$mapscale,
                           style   = styles$hide)

  return(basemap)
}
