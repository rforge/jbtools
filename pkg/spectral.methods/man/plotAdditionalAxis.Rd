\name{plotAdditionalAxis}
\alias{plotAdditionalAxis}
\title{Add a second plot axis with transformed label values}
\description{This function adds a second axis additional labels to a plot. It uses the
axis values of the opposite side and mathematically transforms these into the
values added. This can be used for example to indicate the period and frequency
of a periodic signal. }
\usage{plotAdditionalAxis(side = 1, trans.fun, label = c(), ...)}\arguments{
  \item{side}{integer: which axis to use as a basis for the second one.}
  \item{trans.fun}{function: the transfer function to use between the two axis values.}
  \item{label}{character: labels of the axis.}
  \item{\dots}{further arguments to pass to the axis call.}
}

\value{Nothing is returned.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{axis}}}

