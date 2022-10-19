#' Divides a set of quantitative values into qualitative segments based on a particular strategy.
#'
#' @description
#' Quantitative values often need to be divided into categories. How the
#' breaking into segments occurs depends on the goal of the visualization.
#' 
#' This function provide for seven alternative strategies.
#' - topbottom: divide the range in half and assign row.
#' - fiftyfifty: half the rows into the first group and the top in the second.
#' - quartile3: top and bottom quartiles with the two middle as the other group.
#' - quartile4: each quartile a separate group.
#' - statistics3: below and above one standard deviation plus the middle.
#' - statistics4: like statistics3 but separate the middle at the mean.
#' - cutlist: use cut list of values for cut locations (min and max added automatically).
#' - breaks: use the value of the cuts for the number of segments
#' 
#' This function doesn't use the entire datatable. Instead, just a single column
#' of values is passed to the function (quant_var) along with a strategy for
#' determining the cut locations (i.e., one of the seven listed above). If the breaks
#' strategy is specified, the cuts parameter will be a
#' list of cut locations at which the data values will be divided.
#' 
#' The returned value is a list with one row for each of the data value rows. This
#' list has an index number representing the category for the row value.
#'
#' @param quant_var The table column used as the basis of the segmentation
#' @param cuttype The strategy to be used
#' @param cuts The list of breakpoints (cutlist) or number of cuts (breaks)
#'
#' @return List of index values (1 to nrows) for the segments
#' @export
#'

site_cuts <- function(quant_var, cuttype, cuts = NA){
  
  ## Initialize a bunch of values
  cut_index <- NA
  min <- max <- q <- stdev <- mean_val <- breaks <- NULL
  
  ## Make sure the cuttypes are lowercase
  cuttype <- tolower(cuttype)
  
  ## Make sure that the quant_var is numeric
  quant_var <- as.double(quant_var)
  
  ## Do a few basic calculations that may be needed
  min      <- min(quant_var)
  max      <- max(quant_var)
  q        <- quantile(quant_var)
  stdev    <- sd(quant_var)
  mean_val <- mean(quant_var)
  
  
  ## Process each of the cuttypes separately
  
  if(cuttype == "topbottom"){
    cut_index <- cut(quant_var,
                     breaks = 2, ## half the range
                     labels = FALSE)
  }
  
  if(cuttype == "fiftyfifty"){
    median_break <- q[[3]]  ## median
    cut_index <- cut(quant_var,
                     breaks = c(min,median_break,max),
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "quartiles3"){
    q3breaks <- c(min, q[[2]],q[[4]],max)
    cut_index <- cut(quant_var,
                     breaks = q3breaks,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "quartiles4"){
    q4breaks <- c(min, q[[2]],q[[3]],q[[4]],max)
    cut_index <- cut(quant_var,
                     breaks = q4breaks,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "statistical3"){
    lower <- mean_val - stdev
    upper <- mean_val + stdev
    s3breaks <- c(min, lower, upper, max)
    cut_index <- cut(quant_var,
                     breaks = s3breaks,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "statistical4"){
    lower <- mean_val - stdev
    upper <- mean_val + stdev
    s4breaks <- c(min, lower, mean_val, upper, max)
    cut_index <- cut(quant_var,
                     breaks = s4breaks,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "cutlist"){
    cuts <- append(min,cuts)
    cuts <- append(cuts,max)
    cut_index <- cut(quant_var,
                     breaks = cuts,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  if(cuttype == "breaks"){
    cut_index <- cut(quant_var,
                     breaks = cuts,
                     labels = FALSE,
                     include.lowest = TRUE)
  }
  
  return(cut_index)
  
}