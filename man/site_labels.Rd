% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site_labels.R
\name{site_labels}
\alias{site_labels}
\title{Add labels to point locations on a basemap layer.}
\usage{
site_labels(datatable, styles = column)
}
\arguments{
\item{datatable}{The table with the text, location and (optionally) style data}

\item{styles}{A dataframe with style values for each of the map characteristics}
}
\value{
Data supplied to ggmap that places a layer on the basemap
}
\description{
It is common to mark important locations with points. Labels add information
about each point.

Each label's style can be customized in many ways, including size, background color,
surrounding line, and more.

Label style customization can be done with a constant value for label style columns (e.g., column$lable_text_color)
or (more commonly) by using
a column in the datatable with the same name (e.g., label_text_color). By using a datatable
column, you can have the label style for each point to have its own
characteristic.

A dataframe (called column) stores a set of default style values. Load this
set of styles by using the function site_styles(). This MUST be loaded before
you make a map, even if you are using only datatable style values.
}
