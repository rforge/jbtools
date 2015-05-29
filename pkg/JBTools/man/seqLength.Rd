\name{seqLength}
\alias{seqLength}
\title{Determine lengths of sequences of identical elements }
\description{The function returns a vector indicating the length of sequences of identical 
elements or elements of an identical type in a vector.}
\usage{seqLength(array_in, funct.seq = is.na)}
\arguments{
  \item{array_in}{numeric: input vector}
  \item{funct.seq}{string or name of a function: name of the function to use. Has to return
TRUE for elements belonging to the sequence and FALSE otherwise}
}
\details{The function returns a vector of the same length as the input vector indicating
for each element in array_in how long the sequence of similar elements this value 
belongs to id. Zero indicates that identical values (or types) in the neighbourhood 
of the value. This process helps to indicate for
example long gaps in timeseries.}
\value{numeric vector of the same length as the input vector indicating the gap length
for each single element}

\author{Jannis v. Buttlar}



\seealso{\code{\link{seqLongest}}}

