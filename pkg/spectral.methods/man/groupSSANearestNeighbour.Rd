\name{groupSSANearestNeighbour}
\alias{groupSSANearestNeighbour}
\title{Group SSA eigentriples by finding nearest euclidian neighbours}
\description{This function finds groups in SSA eigentriples by reconstructing
individual eigentriples back to their original (time)  domain and by
determining the nearest euclidian neighbour for each eigentriple.
Groups with more than two members are constructed by identifying
groups with a very similar Euclidian distance.}
\usage{groupSSANearestNeighbour(x, ...)}\arguments{
  \item{x}{object of class ssa (e.g. the results from a call to ssa)}
  \item{\dots}{other objects that can be passed to the function but which
are not used. This is only implemented to make the function
identical in its call to the clusterify function.}
}

\value{list: list indicating the grouping of the SSA eigentriples}

\author{Jannis v. Buttlar}



\seealso{\code{\link{ssa}}, \code{\link{clusterify}}}

