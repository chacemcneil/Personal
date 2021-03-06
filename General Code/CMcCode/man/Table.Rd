% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UsefulFunctions.R
\name{Table}
\alias{Table}
\title{Total Table Function}
\usage{
Table(..., along = NULL, prop = FALSE)
}
\arguments{
\item{...}{Objects passed to \code{table}}

\item{along}{which dimensions to calculate totals for.}

\item{prop}{whether to show the results as proportions. Default is FALSE.}
}
\description{
Similar to \code{table} but adds row and column totals.
}
\examples{
x <- c(1, 2, 2, 2, 2, 3, 3, 3)
y <- c(1, 1, 2, 2, 3, 3, 3, 4)
table(x, y)
Table(x, y)
Table(x, y, along = 2)
Table(x, y, along = 2, prop = T)
}
