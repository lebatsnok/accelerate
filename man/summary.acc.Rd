% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate-summarize.R
\name{summary.acc}
\alias{summary.acc}
\title{summary.acc}
\usage{
\method{summary}{acc}(
  object,
  BY = "DSTday",
  STATS = if (short) list(function(...) CNTSTATS(..., short = TRUE)) else c(CNTSTATS,
    STEPSTATS, HRSTATS),
  copy = c("File", "ID", "Epoch", "Model", "Serial"),
  report.errors = !short,
  roundto = NULL,
  short = FALSE,
  ...
)
}
\arguments{
\item{object}{x}

\item{BY}{an interval specification that can be interpreted by cut.POSIXt, e.g., "DSTday" or "30 mins" or "2 hours"}

\item{STATS}{STATS}

\item{copy}{the columns to be copied from the header part of the acc file, defaults to c("File", "ID", "Epoch", "Model", "Serial")}

\item{report.errors}{report.errors}

\item{roundto}{only used if BY=="30 mins": any value that is not NULL will cause the time stamps to be rounded to nearest half-hour (this was quick hack for synchronizing data with another device -- let me know if you need something like this)}

\item{short}{use short output (no HR or error reporting, and just one set of cutoffs)}

\item{...}{...}
}
\value{
summary.acc
}
\description{
summary.acc
}
