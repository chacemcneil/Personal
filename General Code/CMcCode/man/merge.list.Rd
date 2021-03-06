% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UsefulFunctions.R
\name{merge.list}
\alias{merge.list}
\title{Merge lists}
\usage{
\method{merge}{list}(..., priority = c("first", "last"))
}
\arguments{
\item{...}{Lists to be merged. Elements of each list should be named to avoid confusion.}

\item{priority}{Designates which entry should be kept if lists have elements of the same name.}
}
\description{
Creates a new list with entries from multiple lists. Entries with common names are overwritten
according to a certain priority.
}
\examples{
x <- list(a = 1:5, b = LETTERS[1:3])
y <- list(b = c("X", "Y", "Z"), c = TRUE)
merge.list(x, y)
merge.list(x, y, priority = "last")
}
