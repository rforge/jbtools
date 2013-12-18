\name{isSeriesConstant}
\alias{isSeriesConstant}
\title{check whether series is constant}
\description{This function checks whether a series is constant (up to a certain degree).}
\usage{isSeriesConstant(x, tresh.const = 1e-12, ratio.const = 0.05)}
\arguments{
  \item{x}{numeric vector: series to test.}
  \item{tresh.const}{numeric: maximum deviation allowed which is still
considered to be constant.}
  \item{ratio.const}{numeric: ratio of the series which is allowed to be
not constant for the whole series to be still
considered to be constant.}
}

\value{logical: TRUE if series is constant, FALSE otherwise.}

\author{Jannis v. Buttlar}





