\name{registerParallel}
\alias{registerParallel}
\title{Set up a parallel computing front end}
\description{This function automatically sets up a cluster in a consistent way
way for different parallel computing packages.}
\usage{registerParallel(pckg.parallel = "doMC", max.cores = 0)}\arguments{
  \item{pckg.parallel}{character string: Package to use for parallel
computing. Has to be (for the time beeing) one of doMC or doParallel.}
  \item{max.cores}{integer: amount of cores to use}
}
\details{registerParallel sets up a cluster object of the selected package. In
principle,  this is a simple wrapper around the cluster creating
functions of these packages that provides a unified usage.}
\value{For "doMC": the amount of cores and for doParallel: the cluster object created. }

\author{Jannis v. Buttlar}



\seealso{\code{\link[foreach]{foreach}}, \code{\link[doMC]{registerDoMC}}}

