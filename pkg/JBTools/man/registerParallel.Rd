\name{registerParallel}
\alias{registerParallel}
\title{set up parallel computing front end}
\description{This function automatically sets up the system so that
parallel computing is possible for doMC (yet).}
\usage{registerParallel(pckg.parallel = "doMC", max.cores = 0)}
\arguments{
  \item{pckg.parallel}{character string: package to use for parallel
computing. Has to be one of doMC.}
  \item{max.cores}{integer: amount of cores to use}
}



\author{Jannis v. Buttlar}



\seealso{\code{\link[foreach]{foreach}}, \code{\link[doMC]{registerDoMC}}}

