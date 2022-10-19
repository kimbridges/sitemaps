#' Set all the default style values used for basemaps and annotation.
#'
#' @description
#'
#' Load these data early in the code. Once these are set, you can override any
#' columneter with a separate line of code, such as:
#'
#'   `column$point_color <- "yellow"`
#'
#'Note that the names of the styles (e.g., `label_text_size`, `point_color`) are the names
#'of columns in the data table if you are specifying separate values for each
#'style in the table.
#'
#' Consult the vignettes for information on the use of these styles
#' and the alternative values they may take.
#'
#' @returns A set of values which are used as defaults
#' @export
#'
#' @examples
#' column <- site_styles()
#'
site_styles <- function(){

  column <- NULL          ## Needed to get started

  #################################################################
  ## Label columns
  ## Note: "lines" is a unit of measure for some dimensions (default)
  column$label_text_size           <- 4             ## text size
  column$label_text_color          <- "black"       ## text color
  column$label_alpha               <- 0.9           ## transparency (0 = transparent)
  column$label_corner_radius       <- unit(0.3, "lines") ## rounded corner radius
  column$label_outline_thickness   <- 0.5           ## width of the label border (mm)
  column$label_background_color    <- "white"       ## label background color
  #column$box_padding   <- 0.8           ## from point location
  #column$point.padding <- 0             ## space around point
  column$label_connector_color     <- "red"         ## line from point to label
  column$label_connector_thickness <- 1             ## line width from label to point
  column$seed                      <- 1002          ## randomizes without a value
  column$label_text_wrap           <- NA            ## text column wrap (NA = don't wrap)

  #################################################################
  ## Point columns
  column$point_size               <- 3            ## symbols size
  column$point_alpha              <- 0.9          ## point transparency
  column$point_color              <- "red"        ## inside color
  column$point_shape              <- "circle"     ## circle (symbol 21)
  column$point_outline_color      <- "black"      ## outside border
  column$point_outline_thickness  <- 1            ## border thickness

  #################################################################
  ## Names columns
  column$name_text_size           <- 4         ## text size
  column$name_text_color          <- "black"    ## text color
  column$fontface                 <- "plain"    ## alt: italics, bold
  column$family                   <- "sans"     ## sans-serif typeface (likely Helvetica)
  column$angle                    <- 0          ## negative is clockwise
  column$justify                  <- "center"   ## horizontal justification
  column$name_text_wrap           <- NA    ## text column wrap (NA = don't wrap)

  #################################################################
  ## Legend columns
  column$leg_point_size           <- 3        ## point size
  column$leg_point_color          <- "black"  ## point color
  column$leg_point_shape          <- "circle" ## point shape
  column$leg_outline_color        <- "black"  ## point outline color
  column$leg_outline_thickness    <- 0        ## point outline thickness
  column$leg_meaning_face         <- "plain"  ## font face for the point meanings
  column$leg_meaning_text_size    <- 12       ## point size for the meaning text
  column$leg_meaning   <- ""       ## meaning of the point symbolism
  column$leg_attribute <- ""       ## text for the header line
  column$leg_header_face          <- "plain"  ## font face for the header lines
  column$leg_header_text_size     <- 12       ## point size for the header text
  
  #################################################################
  ## Scale columns
  column$scale_line_thickness  <- 2        ## thickness of the scale bar
  column$scale_text_size       <- 4        ## size of the text with the dimension and unit
  column$scale_anchor          <- "lr"     ## anchor points for scale line (ur, ul, ll, lr)
  column$scale_margin          <- 0.1      ## percent of the width to leave as a margin
  column$scale_width           <- 0.20     ## use about this percent of the width for the scale bar
  column$metric                <- TRUE     ## FALSE for English (ft, miles); TRUE for metric (m, km)
  column$scale_line_color      <- "black"  ## scale bar color
  column$scale_text_color      <- "black"  ## dimension and unit text color
  
  #################################################################
  ## Google Maps constants
  column$gmaptype   <- "roadmap"
  column$mapscale   <- 2    ## Max available
  column$hide       <- "feature:poi|element:labels|visibility:off" ## hide poi
  column$margin     <- 0  ## expand bounding box (0=points at margin)

  #################################################################
  ## Stamen Maps constants
  column$smaptype <- "terrain"
  column$square   <- FALSE  ## TRUE forces a square bounding box

  #################################################################
  ## Fans constants
  column$fan_direction     <- 90      ## degrees
  column$fan_field_width   <- 45      ## degrees (the field-of-view)
  column$fan_arm_length    <- 100     ## meters
  column$fan_arm_thickness <- 1       ## width of each arm 
  column$fan_arm_color     <- "black" ## arm color
  column$fan_arm_linetype  <- "solid" ## standard line type names
  column$fan_color         <- "red"   ## color of the fill between arms
  column$fan_alpha         <- 0.6     ## alpha transparency of the fill (0-1)
  column$fan_fill          <- TRUE    ## TRUE = fill; FALSE = no fill
  
  #################################################################
  return(column)
}
