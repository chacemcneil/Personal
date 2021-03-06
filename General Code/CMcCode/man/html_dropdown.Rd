% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkdownFunctions.R
\name{html_dropdown}
\alias{html_dropdown}
\title{HTML Dropdown Function}
\usage{
html_dropdown(id, divs, labels = NULL, onchange = paste0(id,
  "_function(value);"), onload = "")
}
\arguments{
\item{id}{Character, id of the dropdown menu. Must be unique from other ids}

\item{labels}{Vector of values to display in dropdown list. Will be converted to \code{character}}

\item{values}{Values to associate with labels. If NULL (default), names of \code{labels} will be used. Can be character or numeric.}
}
\description{
Creates HTML dropdown list containing the elements of a vector.
}
\examples{
x <- c(first = 10, second = 20, third = 30)

# Dropdown list with default values
html_dropdown("tmp", x)

# Dropdown list with selected values
html_dropdown("tmp", x, values = 1:4)
}
