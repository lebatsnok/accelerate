% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate-reading.R
\name{read.actigraph.dat}
\alias{read.actigraph.dat}
\title{Read in actigraph *.dat file}
\usage{
read.actigraph.dat(
  file,
  DIR,
  id.prefix = "",
  preprocess = delete.zeros,
  dateformat = c("month", "day", "year"),
  get.id = getID,
  .filter = FILTER
)
}
\arguments{
\item{file}{the file to read}

\item{DIR}{folder - added to 'file' but ignored if 'file' contains '/'}

\item{id.prefix}{- prefix to be added to ID (this argument will be removed in future versions as the same functionality can be achieved with writing acustom get.id function)}

\item{preprocess}{- function to preprocess data - either delete.zeros or reintegrate - or a custom function}

\item{dateformat}{- c("month", "day", "year") - order in which  each element occurs in the date string}

\item{get.id}{- function to extract ID from file name, eg. getID}

\item{.filter}{- function to filter out implausible values}
}
\value{
an object (list) of class "acc"
}
\description{
Reads in an Actigraph (GT1M, ActiTrainer) dat file and
returns an object of class "acc" that can be further
processed with summary.acc etc.
}
\details{
For reading in older 'Actigraph' files in "dat" format.
The main shortcoming of this format is ambiguity of date format -
it is dependent on computer used to download the data. Therefore
the newer format (agd) should be used when possible.
}
\seealso{
\code{\link{read.actigraph.csv}}
}
