#' Create a legend to explain the symbolism used with the map points.
#'
#' @description
#' There are many ways map points can be colored, sized, and outlined.
#' These attributes provide visual cues to information. The link between
#' the point attributes and the meaning is made in the map legend.
#' 
#' The legend is constructed from a rows of information. Some rows have
#' descriptive text, such as identifying a category of points, such as
#' points of different sizes representing population sizes. The other rows
#' have a point symbol and a short text statement that describes the 
#' attribute associated with the specific symbolism. For size, there may 
#' be a set of different size points with the terms small, medium and
#' large.
#' 
#' The function uses two tables as primary input. The header table (h_table) has
#' the text lines. The point table (p_table) has the symbol information. Each
#' table has line numbers so that the header text and points are properly
#' merged to make the legend.
#' 
#' Values that are constant in a table can be entered (or defaulted) using
#' the column values.
#'
#' Note that this function does not handle legend information for labels.
#'
#' @param h_table A data frame with row number, text, and styles for each header row
#' @param p_table A data frame with row number, point characteristics, and meaning
#' @param styles A dataframe with style values for each of the map characteristics
#' 
#' @return Plot with the map legend
#' @import tibble stringr ggplot2
#' @export
#'
#'

site_legend <- function(h_table, p_table, styles = column){

## Create a legend for a map.
## This uses data from two tables, h_table for text lines
## and p_table for the map points and the associated meaning.
  
## Note that label symbols are not handled with this function.
## They can be added if there is any demand for this capability.
  
  ## Variable binding
  number <- leg_attribute <- leg_header_face <- leg_meaning <- NULL
  . <- leg_meaning_face <- NULL

## Fill in any missing columns with default values
## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
h_table <- h_table %>%
  add_column(!!!styles[!names(styles) %in% names(.)])
p_table <- p_table %>%
  add_column(!!!styles[!names(styles) %in% names(.)])

## Note the following convention for symbol shapes.
## You can use circle, square, diamond, triangle,& wedge.
## These are mapped to pch 21-25.

p_table$leg_point_shape <- sub("circle",  "21",p_table$leg_point_shape)
p_table$leg_point_shape <- sub("square",  "22",p_table$leg_point_shape)
p_table$leg_point_shape <- sub("diamond", "23",p_table$leg_point_shape)
p_table$leg_point_shape <- sub("triangle","24",p_table$leg_point_shape)
p_table$leg_point_shape <- sub("wedge",   "25",p_table$leg_point_shape)

p_table$leg_point_shape <- as.numeric(p_table$leg_point_shape)

l_plot <- ggplot() +
  xlim(min=0.8, max=3) +
  theme_void() +
  geom_text(data=h_table, aes(x=1, y=number,
                              label    = leg_attribute,
                              fontface = leg_header_face),
            size=h_table$leg_header_text_size/.pt,
            hjust=0) +
  geom_text(data=p_table, aes(x=1, y=number,
                              label    = leg_meaning,
                              fontface = leg_meaning_face),
            size=p_table$leg_meaning_text_size/.pt,
            hjust=0,
            nudge_x = 0.1) +
  geom_point(data=p_table, aes(x=1, y=number),
             fill   = p_table$leg_point_color,
             size   = p_table$leg_point_size,
             color  = p_table$leg_outline_color,
             stroke = p_table$leg_outline_thickness,
             shape  = p_table$leg_point_shape) +
  scale_y_reverse() +
  theme(legend.position="none")

  return (l_plot)

} 
