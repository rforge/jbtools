\name{filterTSeriesSSA}
\alias{filterTSeriesSSA}
\title{Decompose a vector (i.e. time series) into spectral bands}
\description{This function decomposes (or filters) a time series into a set of orthogonal (i.e. additive)
components with variance on different and distinct timescales (i.e. within different bands).
It uses the fast and optimized  Singular Spectrum Analysis (SSA) method of Korobeynikov (2010).}
\usage{filterTSeriesSSA(series, borders.wl, M = rep(floor(length(series)/3), 
    times = n.steps), n.comp = rep(40, times = n.steps), harmonics = rep(0, 
    times = n.steps), tolerance.harmonics = 0.05, var.thresh.ratio = 0.005, 
    grouping = c("grouping.auto", "groupSSANearestNeighbour")[1], 
    groupingMethod = "wcor", repeat.extr = rep(1, times = length(borders.wl)), 
    recstr.type = "subtraction", pad.series = c(0, 0),
    SSA.methods = c("nutrlan", "propack", "eigen", "svd"),
    center.series = TRUE, call.freq = quote(calcFrequency(series.t)), 
    n.steps = switch(class(borders.wl), list = length(borders.wl), 
        dim(borders.wl)[2]), plot.spectra = FALSE, second.axis = TRUE, 
    open.plot = TRUE, print.stat = TRUE, xlim = c(), debugging = FALSE, 
    ...)}
\arguments{
  \item{series}{numeric vector: Input time series (no gaps!)}
  \item{borders.wl}{list of numeric vectors: Borders of the different periodicity
bands to extract. Units are the sampling frequency of the series
(needs one  vector per step (see details)).}
  \item{M}{integer (vector): Window length
or embedding dimension (see details and ?ssa) (in ssa() this parameter
is called L).}
  \item{n.comp}{integer (vector): Amount of SSA components to compute.
See the help of ssa (package Rssa) for details.}
  \item{harmonics}{integer (vector): How many harmonics to include in each
component (see details). No harmonics are used if set to 0 (default).}
  \item{tolerance.harmonics}{numeric fraction (0-1): Tolerance to use to determine the
width of the bands the harmonics are looked for in. The actual
width is calculated by multiplying the frequency of the "main"
oscillation with this ratio. Use higher values for oscillations
with few repetitions (and, hence, wider peaks in a spectrum) and lower
ones with those with many repetitions (and thus sharper peaks).}
  \item{var.thresh.ratio}{numeric fraction(0-1): Variance threshold below which eigentriples are
treated as "noise" and will not be included in the groups. The actual
threshold is calculated by multiplying the total variance of
the time series with this fraction.}
  \item{grouping}{character string: Method to use for grouping
the individual SSA eigentriples. 'grouping.auto' uses the function
of that name in package Rssa, 'groupSSANearestNeighbour' employs a rather crude scheme
based on finding pairs (or larger groups) in an euclidian
distance matrix of the reconstructions of all extracted SSA eigentriples.
See ?grouping.auto or ?groupSSANearestNeighbour for details.}
  \item{groupingMethod}{
}
  \item{repeat.extr}{integer value/vector: How often to repeat the
extraction. If the respective value is > 1 than the result of the extraction
is again subject to spectral decomposition/filtering for n times and only
the (filtered) result is treated as the actual band (see details). }
  \item{recstr.type}{string: How to determine the high frequency residuals.
If == 'subtraction', the high frequency signal is computed by subtracting
all other signals from the original series (see details). For all other
values only extracted eigentriples with high frequencies are grouped
in this band.}
  \item{pad.series}{integer vector (length 2): Length of the part of the series to use for
padding at the start (first value) and at the end of the series. Values
of zero (default) cause no padding.}
  \item{SSA.methods}{character vector: Methods to use for the
SSA computation. First the first method is tried, when convergence
fails, the second is used and so on. See the help of ssa() in
package Rssa for details on the methods. It is preferable to use more
than one method as some methods (especially nutrlan) in some cases do not
converge. The last two methods are relatively slow.}
  \item{center.series}{logical: Whether to center the series around zero prior to the computation.
The (subtracted) mean will be added to the long term 'trend' component,
e.g. to the step containing an Inf value in  borders.wl. Not centering
of the series may cause erroneous trend extraction in some cases!}
  \item{call.freq}{'quoted' function call : call to a function to compute
the frequency of the 'major' oscillation present in some time series.
This is used to compute the frequency of the (grouped) SSA eigentriples.
See the help for 'calcFrequency' for details of the default mechanism.}
  \item{n.steps}{integer:
Amount of steps in the process. This argument is only kept
for backwards compatibility. Do not supply or change any values!}
  \item{plot.spectra}{logical: Whether to plot pseudo spectra for the different steps.}
  \item{second.axis}{logical: Whether to plot a second axis with period units}
  \item{open.plot}{logical: Whether to open a new plotting window for the plots. Set this to
FALSE and open and set up a device prior to running the function to specify
or change the device options.}
  \item{print.stat}{logical: whether to print status information during the calculations.}
  \item{xlim}{numeric vector: x-limits for the plotted spectra. If not supplied
it goes from 1 / n....0,5.}
  \item{debugging}{logical: if TRUE, workspaces are saved that can be used for debugging
non convergence cases that do not cause R errors but may indicate
a possible error in the settings, data or code.}
  \item{\dots}{miscellaneous: further arguments passed to the plotting routines.}
}
\details{Purpose
The function is based on "singular spectrum analysis" (SSA) (Golyandina et al. (2001))
based on the Rssa package (Korobeynikov (2010), see Golyandina et al. (2013) for a basic introduction).

Definition of the period borders (borders.wl):
borders.wl contains the borders of the different periodicity bands to
extract. Units are sampling frequency of the series. borders.wl has to be a
list with one element per desired decomposition step (see also 'stepwise
extraction').  In the case of a single band to be extracted, this vector has
to consist of two values (e.g. c(<lower border>,<upper.border>)).  In the
case of the extraction of several bands per step, the upper border of each
band is automatically the lower border of the next band. A value of c(0, 10,
100) would, hence, result in the extraction of two bands, one consisting of
eigentriples with periods between 0 and 10 timesteps and one with periods
between 10 and 100 timesteps. As a result the vector has to consist of one
more value than desired groups. If the vector starts with 0 (and recstr.type
= 'subtraction') all high frequency oscillations are combined this group. Use
the value INF as the upper band for the long term trend (if desired)  to savely
include all low frequency parts here.

Window length (M):
For optimal detection of spectral components the window length
or embedding dimension M should be an integer multiple of the period of the
oscillation that is to be extracted. See also ?ssa (here the same parameter is
called L)

Padding (pad.series):
For padding, the series should start and end exactly at the start and end of a
major oscillation (e.g. a yearly cycle). Additionally the length to use for padding
should be a integer multiple of this period length. The padding is solved internally
by adding the indicated part of the series at the start and at the end of the
series. This padded series is only used internally and only the part of the
series with original data is returned in the results.

High frequency part (recstr.type):
Two different ways to compute the high frequency part (e.g. the part with a
frequency between 0 and the lowest border.wl value) are implemented. The
standard way (if recstr.type == 'subtraction') is to sum all eigentriples with
lower frequencies and subtract this sum from the original series to compute
this as the high frequency residual. Otherwise (if (recstr.type !=
'subtraction')) only the eigentriples with such high frequencies that were
actually extracted are used to build this spectral band.

Stepwise extraction:
In general the whole algorithm can be run stepwise. This means that first a
certain spectral band is computed (with all possible harmonies etc. ...) and
subtracted from the original series. Subsequently this process can be
repeated with the residual as often as wanted. This allows for the adaptation
of, for example, M, harmonics, ... to the particular oscillation to be
extracted. Additionally it often this often leads to a clearer signal
separation. To implement several steps, each of M, n.comp and harmonics needs
to be a vector of the length of the amount of steps. For each step the
corresponding element of this vector is used. borders.wl needs to contain one
list entry per step which needs to be a vector containing all borders of the
bands to extract in this step (see 'Definition of the period borders').

Repeated extraction (repeat.extr):
Especially for the trend it may be advisable to repeat the filtering step for
this particular band. In this case the result of the first filtering
(e.g. the sum of all eigentriples within this band) is filtered again n times
with the same period borders. Finally only the final filtered components are
summed and treated as the actual spectral band. In many cases this is helpful
to exclude high frequency parts from the trend or low frequency components.
It is only possible to repeat the extraction for steps where single bands are
extracted.

Visualize results (plot.spectra):
In the case that diagnostic plots should be plotted (plot.spectra == TRUE)
one (or more) pseudospectra are plotted. Each point in these represents one
group of eigentriples determined. For each step a separate plot is
produced. Colored regions represent the specified spectral bands. grey lines in
the background represent a Fourier Spectrum of the original series.}
\value{list with components
\item{dec.series}{numeric matrix: decomposed timeseries. Each row of this
matrix contains one spectrally filtered component. There
are as many rows as period borders (borders.wl) were
defined. }
\item{borders}{numeric matrix: The lower (first column) and upper
(second column) period borders of each filtered component.
The rows here correspond to the rows in "dec.series".}
\item{settings}{list: Settings used to perform the calculation.}}
\references{Golyandina, N.; Nekrutkin, V.; Nekrutkin, V. & Zhigljavsky, A. (2001),
Analysis of time series structure: SSA and related techniques, CRC Press    
Korobeynikov, A. (2010), Computation- and space-efficient implementation of SSA.
Statistics and Its Interface, Vol. 3, No. 3, Pp. 257-268
Golyandina, N. & Korobeynikov, A. (2013), 'Basic Singular Spectrum Analysis
and forecasting with R', Computational Statistics & Data Analysis.}
\author{Jannis v. Buttlar}



\seealso{\code{\link{ssa}},\code{\link{calcFrequency}}}
\examples{
#create series consisting of two oscillations and noise
series.ex <- sin(2 * pi * 1:1000 / 100) +  0.7 * sin(2 * pi * 1:1000 / 10)  +
  rnorm(n = 1000, sd = 0.4)

#prepare graphics
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8), ncol = 2))
par(tcl = 0.2, mgp = c(2, 0, 0), mar = c(0, 4, 0, 0), oma = c(2, 0, 2, 0),
    ps = 10, cex = 1)
plot.new()

#perform decomposition
data.decomposed <- filterTSeriesSSA(series = series.ex,
    borders.wl = list(a = c(8, 12), b = c(80, 120)
        , c = c(0, 10, 100, Inf)),
    M = c(30, 200, 100),
    n.comp = c(10, 20, 20),
    harmonics = c(1, 0, 0),
    plot.spectra = TRUE, open.plot = FALSE)

#plot series and spectral parts
plot(series.ex)
plot(data.decomposed$dec.series[1, ], ylab = '')
plot(data.decomposed$dec.series[2, ], ylab = '')
plot(colSums(data.decomposed$dec.series[-c(1:2), ]), ylab = '')
mtext(side = 2, outer = TRUE, at = -(1 / 8) + ((4:1) * (1 / 4)),
    c('orig.series', '1.step', '2.step', '3.step'), las = 3, cex = 1.5, line = -1)
mtext(side = 3, outer = TRUE, at = -(1 / 4) + ((1:2) * (1 / 2)),
    c('pseudospectra', 'identified components'), las = 1, cex = 1.5, line = 1)

}

\keyword{SSA,}
\keyword{time}
\keyword{series,}
\keyword{spectral}
\keyword{analysis,}
\keyword{singular}
\keyword{spectrum}
\keyword{analysis,}
\keyword{spectral}
\keyword{decomposition,}
\keyword{filter}
