# sitemaps
An R package with functions to help you add symbols and labels to basemaps.

The mapping uses ggplot2 and ggmap along with basemaps from Google or Stamen. A few other packages are needed as well.

The approach is to move the map specifications to either columns in a data table or, for constant values, to parameters that share the names of table columns.

The intended audience is a person working with reproducible documents (e.g., rmarkdown, quarto) who needs maps to show locations and, often, to link the locations to data values.

There is an attempt to use names for style settings, such as the color of a data point (point_color) that are more intuitive than those used with ggplot2 (fill).
