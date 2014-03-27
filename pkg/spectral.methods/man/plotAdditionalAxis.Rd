\name{plotAdditionalAxis}
\alias{plotAdditionalAxis}
\title{Add second axis with transferred labels}
\description{This function adds a second axis with differing labels to the plot.}
\usage{plotAdditionalAxis(side = 1, trans.fun, label = c(), ...)}\arguments{
  \item{side}{integer: which axis to use as a basis for the second one.}
  \item{trans.fun}{function: the transfer function to use between the two axis values.}
  \item{label}{character: labels of the axis.}
  \item{\dots}{further arguments to pass to the axis call.}
}

\value{Nothing is returned.}

\author{Jannis v. Buttlar, MPI BGC Jena, Germany, jbuttlar@bgc-jena.mpg.de
Jannis v. Buttlar}



\seealso{\code{\link{axis}}}

