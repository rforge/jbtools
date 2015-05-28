\name{calcFrequency}
\alias{calcFrequency}
\title{Estimate the frequency of a periodic signal}
\description{Function to estimate the "dominant" frequency of a periodic time series signal.}
\usage{calcFrequency(series, plot.periodogram = FALSE)}
\arguments{
  \item{series}{numeric vector: input vector (time series)}
  \item{plot.periodogram}{logical: whether to plot a periodogram}
}
\details{This function uses Fourier decomposition to determine the 'major' frequency of a time
series. Technically this is the frequency of the Fourier component with the highest
variance. The function is used by filterTSeriesSSA to determine the frequencies
of the individual SSA components.}
\value{Frequency of the Fourier component with the highest variance [1/time steps]}

\author{Jannis v. Buttlar}



\seealso{\code{\link{fft}},\code{\link{filterTSeriesSSA}}}

