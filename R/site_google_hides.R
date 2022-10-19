#' Load a set of parameter values used to hide layers on Google Maps basemaps.
#'
#' @description
#' Google generated basemaps often contain layers of information, such as
#' road names, points of interest, and landscape features. These layers can
#' generally be hidden.
#'
#' This function brings in code that tells the Google Map server to suppress the
#' various layers.
#'
#' Load this data early in the code. Note that this does not load with `set_param()`.
#'
#' Consult the vignettes to see how to use these control values.
#'
#' @returns A set of values which can be used to hide map layers
#' @export
#'
#' @examples
#' hide <- site_google_hides()
#'


site_google_hides <- function(){
## Use these codes to hide specific map layers (or all of them)
hide <- NULL  ## Needed
hide$none       <- ""
hide$poi        <- "feature:poi|element:labels|visibility:off"
hide$transit    <- "feature:transit|element:labels|visibility:off"
hide$water      <- "feature:water|element:labels|visibility:off"
hide$road       <- "feature:road|element:labels|visibility:off"
hide$roads      <- "feature:road|element:labels|visibility:off"
hide$landscape  <- "feature:landscape|element:labels|visibility:off"
hide$administrative <- "feature:administrative|element:labels|visibility:off"
hide$all        <- "feature:all|element:labels|visibility:off"

return(hide)
}
