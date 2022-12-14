% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site_google_basemap.R
\name{site_google_basemap}
\alias{site_google_basemap}
\title{Create a custom basemap using Google Maps as the server.}
\usage{
site_google_basemap(datatable, styles = column)
}
\arguments{
\item{datatable}{The table with the location coordinates (as lat and lon)}

\item{styles}{A dataframe with style values for each of the map characteristics}
}
\value{
A basemap that is scaled to hold the data points
}
\description{
A basemap is a key element in making a map. The Google Map server provides
several good choices that show different types of information (e.g., terrain)
on the basemap.
}
