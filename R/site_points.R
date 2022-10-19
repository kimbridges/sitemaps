#' Add points to a basemap layer.
#'
#' @description
#' It is common to mark important locations with points.
#'
#' Each point can be customized in many ways, including size, color,
#' surrounding line, and more.
#'
#' Point customization can be done with point parameters (e.g., column$point_color)
#' when all the points are to have the same style or (more commonly) by using
#' a table column (e.g., point_color) to allow the text for each name to have its own style
#' characteristic.
#'
#' A dataframe (called column) stores a set of default values. Load this
#' set by using the function site_styles(). 
#'
#' @param datatable The table with the text, location and (optionally) style data
#' @param styles A dataframe with style values for each of the map characteristics
#'
#' @return Data supplied to ggmap that places a layer on the basemap
#' @import tibble stringr ggplot2
#' @export
#'
#'
site_points <- function(datatable, styles = column){

  ## Add points to a map (used inside ggmap)
  
  ## Global binding
  . <- lat <- lon <- text <- NULL

  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    add_column(!!!styles[!names(styles) %in% names(.)])

  ## Note the following convention for symbol shapes.
  ## You can use circle, square, diamond, triangle,& wedge.
  ## These are mapped to pch 21-25.

  datatable$point_shape <- sub("circle",  "21",datatable$point_shape)
  datatable$point_shape <- sub("square",  "22",datatable$point_shape)
  datatable$point_shape <- sub("diamond", "23",datatable$point_shape)
  datatable$point_shape <- sub("triangle","24",datatable$point_shape)
  datatable$point_shape <- sub("wedge",   "25",datatable$point_shape)

  datatable$point_shape <- as.numeric(datatable$point_shape)

  ## Generate the labels
  geom_point(data = datatable,
             aes(x = lon, y = lat),
             size          = datatable$point_size,
             color         = datatable$point_outline_color,
             alpha         = datatable$point_alpha,
             fill          = datatable$point_color,
             shape         = datatable$point_shape,
             stroke        = datatable$point_outline_thickness)

}
