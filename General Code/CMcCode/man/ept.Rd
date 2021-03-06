% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UsefulFunctions.R
\name{ept}
\alias{ept}
\title{Evaluate Parsed Text}
\usage{
ept(txt, env = NULL, drop = T, useDT = T)
}
\arguments{
\item{txt}{Text to be evaluated}

\item{env}{Environment in which to evaluate \code{txt}}

\item{drop}{If \code{txt} has length 1, whether to return value rather than list. Default is TRUE}

\item{useDT}{If TRUE, returns multiple results in a \code{data.table} object.}
}
\description{
Evaluates a string as written code
}
\examples{
str <- "a > 2"
a <- 1:5
ept(str)
ept(str, drop = FALSE)

str <- c(first = "a > 2", second = "a \%\% 2 == 0")
a <- 1:5
ept(str)
ept(str, drop = FALSE)
}
