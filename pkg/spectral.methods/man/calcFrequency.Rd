\name{calcFrequency}
\alias{calcFrequency}
\title{determine frequency of time series}
\description{Function to determine the "main" frequency of a time series.}
\usage{calcFrequency(series, plot.periodogram = FALSE)}\arguments{
  \item{series}{numeric vector: input time series}
  \item{plot.periodogram}{logical: whether to plot a periodogram}
}
\details{This function uses Fourier decomposition to determine the 'major' frequency of a time
series. Technically this is the frequency of the Fourier component with the highest
variance. The function is used by filterTSeriesSSA to determine the frequencies
of the individual SSA components.}
\value{frequency of the Fourier component with the highest variance [1/time steps].}

\author{Jannis v. Buttlar}



\seealso{\code{\link{fft}},\code{\link{filterTSeriesSSA}}}

