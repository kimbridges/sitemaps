#' Convert a basemap bounding box into one that is square.
#'
#' @description
#' Bounding boxes are created from the distribution of coordinate values. It is
#' not likely that the resulting bounding box will be square. This might be OK
#' for Stamen basemaps. But Google basemaps are always square and you'll need
#' to make the bounding box square in order to calculate the map center and
#' zoom level.
#'
#' @param bb A numeric vector with four elements representing a lat,lon bounding box
#'
#' @return A numeric vector with bounding box values adjusted to make a square bounding box
#' @export
#'
#' @examples
#' x <- c(left=150.1,bottom=21.3,right=135.2,top=30.5)
#' square <- site_squaremap(bb=x)
site_squaremap <- function(bb){

  ## Make a square boundary box

  x_length <- max(c(abs(bb[[1]]),abs(bb[[3]]))) - min(c(abs(bb[[1]]),abs(bb[[3]])))
  y_length <- max(c(abs(bb[[2]]),abs(bb[[4]]))) - min(c(abs(bb[[2]]),abs(bb[[4]])))

  if (x_length < y_length){
    half_diff <- (y_length - x_length) / 2
    bb[[1]] <- bb[[1]] - half_diff
    bb[[3]] <- bb[[3]] + half_diff
  } else {
    half_diff <- (x_length - y_length) / 2
    bb[[2]] <- bb[[2]] - half_diff
    bb[[4]] <- bb[[4]] + half_diff
  }

  return(bb)
}
