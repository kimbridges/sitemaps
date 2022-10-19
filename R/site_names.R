#' Add names (i.e., words) to a basemap layer.
#'
#' @description
#' Basemaps of come with new, if any, names to mark important locations. It is
#' useful to add names so that a map user is properly oriented.
#' At a minimum, each name has a text value (what will appear on the map) and
#' a pair of coordinate values (lon,lat) showing the location of the text.
#'
#' Each text value can be customized in many ways, including size, color,
#' font face, and font type.
#'
#' Text customization can be done with name styles (e.g., column$name_text_color)
#' when all text is to have the same value or (more commonly) a table column
#' (e.g., name_text_color) is used to allow the text for each name to have its own
#' style characteristics.
#'
#' A dataframe (called column) stores a set of default style values. Load this
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

site_names <- function(datatable, styles = column){
  
  ## Variable binding
  . <- lat <- lon <- text <- NULL

  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    tibble::add_column(!!!styles[!names(styles) %in% names(.)])

  ## Wrap text if there are double percent signs
  datatable$text <- stringr::str_replace(datatable$text,"%%","\n")
  
  ## Wrap text into two or more lines (insert slash-n) unless NA
  for(i in 1:nrow(datatable)){
    if(!is.na(datatable$name_text_wrap[i]))
      datatable$text[i] <- str_wrap(datatable$text[i], 
                                    width = datatable$name_text_wrap[i])
  }

  ## Generate the labels
  geom_text(data = datatable,
            aes(x = lon, y = lat,
                label = text),
            size          = datatable$name_text_size,
            color         = datatable$name_text_color,
            fontface      = datatable$fontface,
            angle         = datatable$angle,
            justify       = datatable$justify,
            family        = datatable$family)

}
