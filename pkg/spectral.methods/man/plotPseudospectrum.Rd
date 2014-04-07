\name{plotPseudospectrum}
\alias{plotPseudospectrum}
\title{Plot and calculate the pseudospectrum of spectrally decomposed SSA eigentriples}
\description{This function plots the pseudospectrum of the results from a SSA run, e.g. it
plots the variance of the individual eigentriples vs. their frequencies. It can also
be used to compute the frequency, variance and period of all SSA eigentriples.}
\usage{plotPseudospectrum(ssa.object, calc.raw.SSA = TRUE, plot.spectrum = TRUE, 
    plot.fourier = TRUE, series.orig = c(), pch = 16, col = "red", 
    show.harmonies = TRUE, label.points = TRUE, label.tresh = 5e-04, 
    call.freq = quote(DetermineFreq(series.t)), print.stat = TRUE, 
    ...)}\arguments{
  \item{ssa.object}{SSA object: the results of a run of ssa().}
  \item{calc.raw.SSA}{logical: Whether to additionally compute the whole spectrum for
all un-grouped eigentriples (my slow the process in case of
long time series.}
  \item{plot.spectrum}{logical: whether to plot the pseudospectrum.}
  \item{plot.fourier}{logical: Whether to plot the Fourier spectrum in the background.}
  \item{series.orig}{numeric vector: original, non decomposed time series
(used to calculate Fourier spectrum). If not supplied,
an object with the saved in ssa.object is searched for in
all active environments.}
  \item{pch}{integer: graphic parameter passed to plot() (?par)}
  \item{col}{character: graphic parameter passed to plot() (?par)}
  \item{show.harmonies}{logical: whether to mark the positions of the harmonies of the oscillation
with the highest variance.}
  \item{label.points}{logical: whether to label the points with period values}
  \item{label.tresh}{numeric: threshold used to label points}
  \item{call.freq}{function to use to determine the frequencies
of the ssa eigentriples.}
  \item{print.stat}{logical: whether to print status information during the
calculations.}
  \item{\dots}{
}
}

\value{list with values
\item{paired}{matrix: frequency, period and variance of the paired reconstruction}
\item{raw}{matrix: frequency, period and variance of the unpaired reconstruction
}}

\author{Jannis v. Buttlar}



\seealso{\code{\link[Rssa]{ssa}}}

