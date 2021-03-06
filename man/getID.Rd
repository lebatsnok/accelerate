% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate-reading.R
\name{getID}
\alias{getID}
\title{Get numberic ID from file name}
\usage{
getID(x)
}
\arguments{
\item{x}{file name (character)}
}
\value{
ID (as integer)
}
\description{
Get ID from file name. All numbers (digits) in file name
will be treated as ID; letters are ignored.
For example, '42abcd.dat' becomes 42.
}
