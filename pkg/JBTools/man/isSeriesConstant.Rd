\name{isSeriesConstant}
\alias{isSeriesConstant}
\title{Check whether a vector is constant}
\description{isSeriesConstant checks whether a series is constant (up to a certain degree).}
\usage{isSeriesConstant(x, tresh.const = 1e-12, ratio.const = 0.05)}\arguments{
  \item{x}{numeric vector: series to test.}
  \item{tresh.const}{numeric: maximum deviation allowed which is still
considered to be constant.}
  \item{ratio.const}{numeric: ratio of the series which is allowed to be
not constant for the whole series to be still
considered to be constant.}
}
\details{isSeriesConstant checks whether the amount of values deviating from the
median of x for a higher value than tresh.const is bigger than ratio.const.}
\value{logical: TRUE if series is constant, FALSE otherwise.}

\author{Jannis v. Buttlar}





