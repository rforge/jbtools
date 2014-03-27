\name{summarizeTemporalGaps}
\alias{summarizeTemporalGaps}
\title{compute statistics of temporal gaps in time series vectors}
\description{This function computes the lengths of gaps in a vector and computes mean and max sizes and a histogram. }
\usage{summarizeTemporalGaps(series, index = rep(0, times = length(series)), 
    results.ext = FALSE)}\arguments{
  \item{series}{vector: input time series}
  \item{index}{index vector (optional): integer values indicating
different periods for which to compute the statistics independently.}
  \item{results.ext}{logical: whether to compute and return detailed information like
sizes of individual gaps and count of a histogram.            }
}



\author{Jannis v. Buttlar}





