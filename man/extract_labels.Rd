% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadData.R
\name{extract_labels}
\alias{extract_labels}
\title{Extract the label attribute from data}
\usage{
extract_labels(dat)
}
\arguments{
\item{dat}{data in the form of a \code{\link[base]{list}}, \code{\link[base]{data.frame}}
or \code{\link[tibble]{tibble}}, or a vector}
}
\value{
list of labels
}
\description{
Extract the label attribute from data
}
\examples{
a <- c(1, 2)
attr(a, "label") <- "b"
identical(extract_labels(a), list(a = attr(a, "label")))
}
