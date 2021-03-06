% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accelerate-process.R
\name{CNTSTATS}
\alias{CNTSTATS}
\title{CNTSTATS}
\usage{
CNTSTATS(
  x,
  Epoch,
  cutoffs = Cutoffs,
  return.MVPA = TRUE,
  return.raw = TRUE,
  return.rel = !short,
  rel.factor = 1,
  return.bouts = !short,
  reintegrate.to = NULL,
  short = FALSE
)
}
\arguments{
\item{x}{x}

\item{Epoch}{epoch}

\item{cutoffs}{cutoffs}

\item{return.MVPA}{TRUE}

\item{return.raw}{TRUE}

\item{return.rel}{TRUE}

\item{rel.factor}{1}

\item{return.bouts}{TRUE}

\item{reintegrate.to}{NULL}

\item{short}{FALSE}
}
\value{
3dnx cntstats
}
\description{
CNTSTATS
}
