#' Convert a bounding box into the data needed by Google Maps to create a basemap.
#'
#' @description
#' Bounding boxes are created from a set of geographic coordinates using the
#' `make_bbox` function (from the ggmap package).
#'
#' The bounding box may not be square. The `bb_square` function (from mapfunctions)
#' fixes that.
#'
#' This function takes a bounding box and returns the center coordinates and
#' the zoom level so you can create a basemap with the `make_google_basemap` function
#' (from mapfunctions).
#'
#' @param bb A numeric vector with four elements representing a lat,lon bounding box
#'
#' @returns A pair of numeric values for the map center and an integer for the zoom level
#' @export
#'
#' @examples
#' x <- c(left=150.1,bottom=21.3,right=135.1,top=35.3)
#' g_info <- bb2google(bb=x)
#' map_center <- g_info$center
#' zoom_level <- g_info$zoom

bb2google <- function(bb){

  ## The bounding box can be produced using make_bbox.
  ## A bounding box has left, bottom, right, top coordinates.
  ## This function finds the map center and zoom level
  ## so these data can be used with a Google map request.

  ## This function assumes a square bounding box as input.

  ## Initialize
  info <- NULL

  ## Find the center point
  center_lon <- ((bb[[3]]-bb[[1]])/2) + bb[[1]]
  center_lat <- ((bb[[4]]-bb[[2]])/2) + bb[[2]]
  info$center <- c(center_lon, center_lat)

  ## Use the zoom scales to estimate the proper zoom level
  zoom_scales <- c(180,90,45,22.5,11.25,5.625,2.813,1.406,0.703,0.352,0.176,0.088,0.044,0.022,0.011,0.005,0.003,0.001,0.0005)

  bb_length <- bb[[3]] - bb[[1]]
  info$zoom <- as.integer(which.min(zoom_scales > bb_length))

  return(info)

}
