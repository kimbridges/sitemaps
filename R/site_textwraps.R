#' Break long labels or names into multiple lines by inserting a new-line character.
#'
#' @description
#' It is often necessary to make a multi-line label or name on a map. Limitations
#' on inserting the line break character (slash-n) into input data means that
#' the data needs to be modified with a function.
#' 
#' Lines in the text column of the table are broken only when the text_wrap
#' has a positive value. NA in this column means don't break the text for this
#' table row.
#' 
#' Note: For this function to work, there must be a text column and a text_wrap column.
#'
#' The function only breaks a string of characters where there is a blank. This
#' means that words are not broken.
#'
#' @param datatable The standard datatable with a text column and a text_wrap column
#'
#' @return datatable with some text modified with inserted slash-n for line breaks
#' @export
#'

site_textwraps <- function(datatable){

  ## Wrap the longer text into two or more lines (insert slash-n)
  for(i in 1:nrow(datatable)){
    if(!is.na(datatable$text_wrap[i]))
      datatable$text[i] <- str_wrap(datatable$text[i], 
                                    width = datatable$text_wrap[i])
    }
  return(datatable)
}
