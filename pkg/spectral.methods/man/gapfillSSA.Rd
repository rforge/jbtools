\name{gapfillSSA}
\alias{gapfillSSA}
\title{Fill gaps in a vector (time-series) with SSA}
\description{gapfillSSA applies the iterative gap filling procedure proposed by Kondrashov and Ghil
(2006) in a fast and optimized way developed by Korobeynikov
(2010). Generally spoken, major periodic components of the time series are
determined and interpolated into gap positions. An iterative cross validation
scheme with artificial gaps is used to determine these periodic components.}
\usage{gapfillSSA(amnt.artgaps = c(0.05, 0.05), DetBestIter = ".getBestIteration", 
    debugging = FALSE, amnt.iters = c(10, 10), amnt.iters.start = c(1, 
        1), fill.margins = FALSE, first.guess = c(), GroupEigTrpls = "grouping.auto", 
    groupingMethod = "wcor", kind = c("auto", "1d-ssa", "2d-ssa")[1], 
    M = floor(length(series)/3), matrix.best.iter = "perf.all.gaps", 
    MeasPerf = "RMSE", n.comp = 2 * amnt.iters[1], open.plot = TRUE, 
    plot.results = FALSE, plot.progress = FALSE, pad.series = c(0, 
        0), print.stat = TRUE, remove.infinite = FALSE, scale.recstr = TRUE, 
    series, seed = integer(), size.biggap = 20, SSA.methods = c("nutrlan", 
        "propack", "eigen", "svd"), tresh.convergence = 0.01, 
    tresh.min.length = 5, z.trans.series = TRUE)}
\arguments{
  \item{amnt.artgaps}{numeric vector: The relative ratio (amount gaps/series length) of
artificial gaps to include to determine the iteration with the best
prediction (c(ratio big gaps, ratio small gaps)). If this is set to
c(0,0), the cross validation step is excluded and the iteration
is run until amnt.iters.}
  \item{DetBestIter}{function: Function to  determine the best outer and inner iteration to
use for reconstruction. If no function is given, the standard way is used.
(see ?.getBestIteration)}
  \item{debugging}{logical: If set to TRUE, workspaces to be used for debugging are
saved in case of (some) errors or warnings.}
  \item{amnt.iters}{integer vector: Amount of iterations performed for the outer and inner
loop (c(outer,inner)).}
  \item{amnt.iters.start}{integer vector: Index of the iteration to start with c(outer, inner). If this
value is > 1, the reconstruction (!) part is started with this iteration. Currently
it is only possible to set this to values > 1 if amnt.artgaps != 0 as this would
cause a cross validation loop. }
  \item{fill.margins}{logical: Whether to fill gaps at the outer margins of the series, i.e. to
extrapolate before the first and after the last valid value. Doing this most
probably produces unreliable results (i.e. a strong build up of amplitude).}
  \item{first.guess}{numeric vector/matrix: First guess for the gap values. The mean/zero is used if
no value is supplied. Has to have the same dimensions and lengths as series.}
  \item{GroupEigTrpls}{character string: Name of the function used to group the eigentriples. This function
needs to take a ssa object as its first input and other inputs as its ... argument.
It has to return a list with the length of the desired amount of SSA groups. Each of
its elements has to be a integer vector indicating which SSA eigentriple(s) belong(s)
to this group. The function 'grouping.auto' uses the methods supplied by the Rssa
package (See argument groupingMethod to set the corresponding argument for the
method). Another possibility is 'groupSSANearestNeighbour' which uses a rather
ad-hoc method of detecting the nearest (Euclidian) neighbour of each eigentriple.
2D SSA automatically uses the nearest neighbor method as grouping was not (yet)
implemented for 2D SSA.}
  \item{groupingMethod}{
}
  \item{kind}{character string: Whether to calculate one or two dimensional SSA (see the
help of ssa()). Default is to determine this automatically by determining
the dimensions of series.}
  \item{M}{integer: Window length  or embedding dimension [time steps]. If not
given,  a default value of 0.33*length(timeseries) is computed. For
2d SSA a vector of length 2 has to be supplied. If only one number is given,
this is taken for both dimensions. (see ?ssa, here the parameter is called L)}
  \item{matrix.best.iter}{character string: Which performance matrix to use (has to be one of
recstr.perf.a, recstr.perf.s or recstr.perf.b (see ?.getBestIteration)).}
  \item{MeasPerf}{character string: Name of a function to determine the 'goodness of fit'
between the reconstruction and the actual values in the artificial
gaps. The respective function has to take two vectors as an input and
return one single value. Set to the "Residual Mean Square Error" (RMSE) by default.  }
  \item{n.comp}{integer: Amount of eigentriples to extract (default if no values are
supplied is 2*amnt.iters[1]) (see ?ssa, here the parameter is called neig).}
  \item{open.plot}{logical: Whether to open a new layout of plots for the performance plots.}
  \item{plot.results}{logical: Whether to plot performance visualization for artificial gaps?}
  \item{plot.progress}{logical: whether to visualize the iterative estimation of the reconstruction process
during the calculations.}
  \item{pad.series}{integer vector (length 2): Length of the part of the series to use for
padding at the start (first value) and at the end of the series. Values
of zero cause no padding. This feature has not yet been rigorously tested!}
  \item{print.stat}{logical: Whether to print status information during the calculations.}
  \item{remove.infinite}{logical: Whether to remove infinite values prior to the calculation.}
  \item{scale.recstr}{logical: whether to scale the reconstruction to sd = 1 at the end of each outer
loop step.}
  \item{series}{numeric vector/matrix: equally spaced input time series or matrix with gaps (gap = NA)}
  \item{seed}{integer: Seed to be taken for the randomized determination of the positions of the
artificial gaps and the nutrlan ssa algorithm. Per default, no seed is set.}
  \item{size.biggap}{integer: Length of the big artificial gaps (in time steps)}
  \item{SSA.methods}{character vector: Methods to use for the
SSA computation. First the first method is tried, when convergence
fails the second is used and so on. See the help of ssa() in
package Rssa for details on the methods. The last two methods
are relatively slow!}
  \item{tresh.convergence}{numeric value: Threshold below which the last three sums of squared differences between
inner iteration loops must fall for the whole process to be considered to have converged.}
  \item{tresh.min.length}{integer: minimum length the series has to have to do computations.}
  \item{z.trans.series}{logical: whether to perform z-transformation of the series prior to
the calculation.  }
}
\details{Artificial Gaps:
The amount of artificial gaps to be included is determined as follows:
amnt.artgaps determines the total size of the artificial gaps to be
included.  The number (0-1) determines the number a relative ratio of the
total amount of available datapoints. To switch off the inclusion of either
small or biggaps, set respective ratio to 0.  In general the ratios determine
a maximum amount of gaps.  size.biggap sets the size of the
biggaps. Subsequently the number of biggaps to be included is determined by
calculating the maximum possible amount of gaps of this size to reach the
amount of biggaps set by amnt.artgaps[1]. The amount of small gaps is then
set according to the ratio of amnt.artgaps[1]/amnt.artgaps[2].

Iteration performance measure:
The DetBestIter function should take any of the RMSE matrices (small/big/all gaps)
as an input and return i.best with best inner loops for each outer loop and h.best
as the outer loop until which should be iterated. Use the default function as a
reference.

Visualize results:
If plot.per == TRUE an image plot is produced visualizing the RMSE between
the artificial gaps and the reconstruction for each iteration. A red dot
indicates the iteration chosen for the final reconstruction.

Padding:
For padding the series should start and end exactly at the start and end of a
major oscillation (e.g. a yearly cycle and the length to use for padding
should be a integer multiple of this length. The padding is solved internally
by adding the indicated part of the series at the start and at the end of the
series. This padded series is only used internally and only the part of the
series with original data is returned in the results.  Padding is not (yet)
possible for two dimensional SSA.

Multidimensional SSA:
1d or 2d SSA is possible. If a vector is given, one dimensional SSA is
computed. In case of a matrix as input, two dimensional SSA is performed. For
the two dimensional case two embedding should be given (one in the direction
of each dimension). If 'big gaps' are set to be used for the cross
validation, quadratic blocks of gaps with the size
'size.biggap'*'size.biggap' are inserted.}
\value{list with components
\item{error.occoured}{logical: whether a non caught error occoured in one
of the SSA calculations.}
\item{filled.series}{numeric vector/matrix: filled series with the same
length as series but without gaps. Gaps at the margins
of the series can not be filled and will occur in
filled.series (and reconstr).}
\item{i.best}{integer matrix: inner loop iteration for each outer loop step in
which the process has finally converged (depending on the
threshold determined by tresh.convergence). If the RMSE
between two inner loop iterations has been monotonously
sinking (and hence, the differences between SSA iterations
can be expected to be rather small), this is set to amnt.iters[2].
If not, the process most likely has been building up itself, this
is set to 0. In both cases iloop.converged is set FALSE.}
\item{iloop.converged}{logical matrix: Whether each outer loop iteration has converged
(see also i.best).}
\item{iter.chosen}{integer vector: iterations finally chosen for the
reconstruction.   }
\item{perf.all.gaps}{numeric matrix: performance (RMSE) for the filling
of all artificial gaps.}
\item{perf.small.gaps}{numeric matrix: performance (RMSE) for the filling
of the small artificial gaps.}
\item{perf.big.gaps}{numeric matrix: performance (RMSE) for the filling
of the big artificial gaps.}
\item{process.converged}{logical: Whether the whole process has converged. For
simplicity reasons, this only detects whether the last outer loop
of the final filling process has converged.}
\item{reconstr}{numeric vector/matrix: filtered series or reconstruction
finally used to fill gaps.}
\item{recstr.diffsum}{numeric matrix: RMSE between two consecutive inner loop iterations.
This value is checked to be below tresh.convergence to determine
whether the process has converged.                                }
\item{settings}{list: settings used to perform the calculation.}}
\references{Kondrashov, D. & Ghil, M. (2006), Spatio-temporal filling of missing points in geophysical data sets,
Nonlinear Processes In Geophysics,S 2006, Vol. 13(2), pp. 151-159
Korobeynikov, A. (2010), Computation- and space-efficient implementation of SSA.
Statistics and Its Interface, Vol. 3, No. 3, Pp. 257-268}
\author{Jannis v. Buttlar}



\seealso{\code{\link[Rssa]{ssa}}}
\examples{
## create series with gaps
series.ex <- sin(2 * pi * 1:1000 / 100) +  0.7 * sin(2 * pi * 1:1000 / 10) +
  rnorm(n = 1000, sd = 0.4)
series.ex[sample(c(1:1000), 30)] <- NA
series.ex[c(seq(from = sample(c(1:1000), 1), length.out = 20),
            seq(from = sample(c(1:1000), 1), length.out = 20))]<-NA
indices.gaps <- is.na(series.ex)

## prepare graphics
layout(matrix(c(1:5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7), ncol = 5, byrow = TRUE), 
       widths = c(1, 1, 1, 0.1, 0.1))
par(mar = c(2, 0, 0, 0.2), oma = c(0, 3, 2, 0.2), tcl = 0.2, mgp = c(0, 0, 100),
    las = 1)

## perform gap filling
data.filled <- gapfillSSA(series = series.ex, plot.results = TRUE, open.plot = FALSE)

## plot series and filled series
plot(series.ex, xlab = '', pch = 16)
plot(data.filled$filled.series, col = indices.gaps+1, xlab = '', pch = 16)
points(data.filled$reconstr, type = 'l', col = 'blue')
mtext(side = 1, 'Index', line = 2)
legend(x = 'topright', merge = TRUE, pch = c(16, 16, NA), lty = c(NA, NA, 1), 
       col = c('black', 'red', 'blue'),
       legend = c('original values', 'gap filled values', 'reconstruction'))
}

\keyword{SSA,}
\keyword{gap}
\keyword{filling,}
\keyword{time}
\keyword{series,}
\keyword{spectral}
\keyword{analysis,}
\keyword{singular}
\keyword{spectrum}
\keyword{analysis}
