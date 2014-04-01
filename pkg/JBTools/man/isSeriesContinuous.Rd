\name{isSeriesContinuous}
\alias{isSeriesContinuous}
\title{Test for continuous (non NA interrupted) series}
\description{isSeriesContinuous test whether a vector contains one non interrupted sequence
of values (i.e. no NaN in between). }
\usage{isSeriesContinuous(x)}\arguments{
  \item{x}{numeric vector: series to test}
}
\details{The function returns TRUE when the vector contains one (and only one)
non NA interupted sequence of values. E.g. it would also return 
TRUE for a vector that contains a sequence of NAs at the beginning
and/or end of the vector. }
\value{logical: whether the series contains gaps or not.}

\author{Jannis v. Buttlar}





