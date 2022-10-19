#' Add labels to point locations on a basemap layer.
#'
#' @description
#' It is common to mark important locations with points. Labels add information
#' about each point.
#'
#' Each label's style can be customized in many ways, including size, background color,
#' surrounding line, and more.
#'
#' Label style customization can be done with a constant value for label style columns (e.g., column$lable_text_color)
#' or (more commonly) by using
#' a column in the datatable with the same name (e.g., label_text_color). By using a datatable
#' column, you can have the label style for each point to have its own
#' characteristic.
#'
#' A dataframe (called column) stores a set of default style values. Load this
#' set of styles by using the function site_styles(). This MUST be loaded before
#' you make a map, even if you are using only datatable style values.
#'
#' @param datatable The table with the text, location and (optionally) style data
#' @param styles A dataframe with style values for each of the map characteristics
#' 
#' @return Data supplied to ggmap that places a layer on the basemap
#' @import tibble stringr ggplot2 ggrepel
#' @export
#'
#'
site_labels <- function(datatable, styles = column){

  ## Add labels to a map (used inside ggmap)
  
  ## Global binding
  . <- lat <- lon <- text <- NULL

  ## Fill in any missing columns with default values
  ## Thanks to Chris Umphlett on StackOverflow (5/3/2019)
  datatable <- datatable %>%
    add_column(!!!styles[!names(styles) %in% names(.)])
  
  ## Wrap text into two or more lines (insert slash-n) unless NA
  for(i in 1:nrow(datatable)){
    if(!is.na(datatable$label_text_wrap[i]))
      datatable$text[i] <- str_wrap(datatable$text[i], 
                                    width = datatable$label_text_wrap[i])
  }

  ## Generate the labels
  geom_label_repel(data = datatable,
                   aes(x     = lon, 
                       y     = lat,
                       label = text),
                   size          = datatable$label_text_size,
                   color         = datatable$label_text_color,
                   alpha         = datatable$label_alpha,
                   label.r       = datatable$label_corner_radius,
                   label.size    = datatable$label_outline_thickness,
                   fill          = datatable$label_background_color,
                   box.padding   = 0.8,
                   segment.color = datatable$label_connector_color,
                   segment.size  = datatable$label_connector_thickness,
                   min.segment.length = 0.01,
                   seed          = styles$seed)

  
}
