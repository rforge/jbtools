\name{isSeriesContinuous}
\alias{isSeriesContinuous}
\title{test for continuous (non NA interupted) series}
\description{The function returns TRUE when the vector contains one (and only one)
non NA interupted sequence of values. E.g. it would also return 
TRUE for a vector that contains a sequence of NAs at the beginning
and/or end of the vector. }
\usage{isSeriesContinuous(x)}
\arguments{
  \item{x}{numeric vector: series to test}
}

\value{logical: whether the series contains gaps or not.}

\author{Jannis v. Buttlar}





