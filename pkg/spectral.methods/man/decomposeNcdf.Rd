\name{decomposeNcdf}
\alias{decomposeNcdf}
\title{Spectrally decompose all time series in a netCDF datacube}
\description{Wrapper function to automatically decompose gridded time series inside a ncdf file and save the results
to another ncdf file using SSA.
}
\usage{decomposeNcdf(file.name, borders.wl, calc.parallel = TRUE, center.series = TRUE, 
    check.files = TRUE, debugging = FALSE, harmonics = c(), M = c(), 
    max.cores = 16, n.comp = c(), pad.series = c(0, 0), print.status = TRUE, 
    ratio.const = 0.05, repeat.extr = rep(1, times = length(borders.wl)), 
    tresh.const = 1e-12, var.names = "auto", ...)}\arguments{
  \item{file.name}{character: name of the ncdf file to decompose. The file has to be in the current working directory!}
  \item{borders.wl}{list: borders of the different periodicity bands to extract. Units are
sampling frequency of the series. In case of monthly data border.wl<- list(c(11, 13))
would extract the annual cycle (period = 12). For details, see the documentation of filterTSeriesSSA.}
  \item{calc.parallel}{logical: whether to use parallel computing. Needs package doMC process.}
  \item{center.series}{SSA calculation parameter: see the documentation of filterTSeriesSSA!}
  \item{check.files}{logical: whether to use checkNcdfFile to check ncdf files for consistency.}
  \item{debugging}{logical: if set to TRUE, debugging workspaces or dumpframes are saved at several stages
in case of an error.     }
  \item{harmonics}{SSA calculation parameter: Number of harmonics to be associated with each band. See the
documentation of filterTSeriesSSA!}
  \item{M}{SSA calculation parameter. Window length for time series embedding (can be different
for each element in borders.wl): see the documentation of filterTSeriesSSA.}
  \item{max.cores}{integer: maximum number of cores to use.}
  \item{n.comp}{SSA calculation parameter: see the documentation of filterTSeriesSSA!}
  \item{pad.series}{SSA calculation parameter: see the documentation of filterTSeriesSSA!}
  \item{print.status}{logical: whether to print status information during the process}
  \item{ratio.const}{numeric: max ratio of the time series that is allowed to be above tresh.const for the time series
still to be not considered constant. }
  \item{repeat.extr}{SSA calculation parameter: see the documentation of filterTSeriesSSA!}
  \item{tresh.const}{numeric: value below which abs(values) are assumed to be constant and excluded
from the decomposition}
  \item{var.names}{character string: name of the variable to fill. If set to 'auto' (default), the name
is taken from the file as the variable with a different name than the dimensions. An
error is produced here in cases where more than one such variables exist.}
  \item{\dots}{additional arguments transferred to filterTSeriesSSA.}
}
\details{This is a wrapper function to automatically load, decompose and save a ncdf file using Singular Spectrum Analysis
(SSA). It facilitates parallel computing and uses the filterTSeriesSSA() function. Refer to
the documentation of filterTSeriesSSA() for details of the calculations and the necessary parameters, especially
for how to perform stepwise filtering.


NCDF file specifications

Due to (possible) limitations in file size the ncdf file can only contain one variable and the one dimensional
coordinate variables. The file has to contain one time dimension called 'time'. This function will
create a second ncdf file identical to the input file but with an additional dimension called 'spectral.bands'
which contains the separated spectral bands. In general the data is internally split into
individual time series along ALL dimensions other than time, e.g. a spatiotemporal data cube would be separated
into individual time series along its longitude/latitude dimension . The individual series are  decomposed
and finally combined, transposed and saved in the new file.

The NCDF file may contain NaN values at
grid locations where no data is available (e.g. ocean tiles) but individual time series from single "valid"
grid points must not contain missing values.
In other words, decomposition is only performed for series without missing values, results for non gap-free series
will be missing_value the results file.

The function has only been exhaustively tested with ncdf files with two spatial dimensions (e.g. latitude and
longitude) and the time dimension. Even though it was programmed to be more flexible, its functionality can not
be guaranteed under circumstances with more and/or different dimensions.
Input NCDF files should be compatible with the Climate Forcasting (CF) 1.5 ncdf conventions. Several crucial
attributes and dimension units are checked and an error is caused if the convention regarding these aspects is
not followed. Examples are the attributes scale_factor, add_offset _FillValue and the units for the time dimension

Parallel computing

If calc.parallel == TRUE, single time series are decomposed with parallel computing. This requires
the package doMC  (and its dependencies) to be installed on the computer.
Parallelization with other packages is theoretically possible but not yet implemented. If
multiple cores are not available, setting calc.parallel to FALSE will cause the process to be
calculated sequential without these dependencies. The package foreach is needed in all cases.}
\value{Nothing is returned but a ncdf file with the results is written in the working directory.
TODO add mechanism to get constant values in datacube after calculation.
TODO Try zero line crossings for frequency determination
TODO Make method reproducible (seed etc)
TODO Add way to handle non convergence
prepare parallel back end
save argument values of call
check input
open ncdf files
set default parameters
determine call settings for SSA
prepare results file
prepare parallel iteration parameters
determine slices to process
create 'iterator'
define process during iteration
perform calculation
add missing value attribute
save results
add attributes with process information to ncdf files}

\author{Jannis v. Buttlar}



\seealso{\code{\link[Rssa]{ssa}}, \code{\link[spectral.methods]{filterTSeriesSSA}}, \code{\link{gapfillNcdf}}}
\examples{
## Example for the filtering of monthly data
filename   <- '<filename>.nc'
# Extract yearly cycle, intra annual part and high frequency residual in several steps
borders.wl <- list(a = c(10, 14)
                   , b = c(12, Inf)
                   , c = c(0, 12))
M         <- c(2*12, 4*12, 12)
#extract first four harmonics for yearly cycle
harmonics <- c(4, 0, 0)

# uncomment and run 
# decomposeNcdf(file.name = filename, borders.wl = borders.wl, M = M, harmonics = harmonics)

# Extract yearly cycle, intra annual part and high frequency residual in one step
borders.wl <- list(c(0,10,14,Inf))
# use the same M for all bands
M          <- c(2*12)
# uncomment and run
#decomposeNcdf(file.name = filename, borders.wl = borders.wl, M = M)
}
