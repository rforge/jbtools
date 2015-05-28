\name{gapfillNcdf}
\alias{gapfillNcdf}
\title{Fill gaps in time series or spatial fields inside a netCDF file using SSA.}
\description{Wrapper function to automatically fill gaps in series or spatial fields
inside a ncdf file and save the results to another ncdf file. This can
be done via 1D,  2D or the spatio - temporal gap 3D filling algorithm
of Buttlar et. al (2014).}
\usage{gapfillNcdf(amnt.artgaps = rep(list(rep(list(c(0.05, 0.05)), 
    times = length(dimensions[[1]]))), times = length(dimensions)), 
    amnt.iters = rep(list(rep(list(c(10, 10)), times = length(dimensions[[1]]))), 
        times = length(dimensions)), amnt.iters.start = rep(list(rep(list(c(1, 
        1)), times = length(dimensions[[1]]))), times = length(dimensions)), 
    calc.parallel = TRUE, debugging = FALSE, debugging.SSA = FALSE, 
    dimensions = list(list("time")), file.name, first.guess = "mean", 
    force.all.dims = FALSE, gaps.cv = 0, keep.steps = TRUE, M, 
    max.cores = 8, max.steps = 10, n.comp = lapply(amnt.iters, 
        FUN = function(x) x[[1]][[1]][1] * 2), ocean.mask = c(), 
    pad.series = rep(list(rep(list(c(0, 0)), times = length(dimensions[[1]]))), 
        times = length(dimensions)), print.status = TRUE, process.cells = c("gappy", 
        "all")[1], process.type = c("stepwise", "variances")[1], 
    ratio.const = 0.05, ratio.test = 1, reproducible = FALSE, 
    size.biggap = rep(list(rep(list(20), times = length(dimensions[[1]]))), 
        times = length(dimensions)), tresh.const = 1e-12, tresh.converged = 0, 
    tresh.fill = 0.1, var.names = "auto", ...)}
\arguments{
  \item{amnt.artgaps}{list of numeric vectors:
The relative ratio (length gaps/series length) of
artificial gaps to include in the "innermost" SSA loop (e.g. the value used by the
SSA run for each individual series/slice). These ratio is used to determine 
the iteration with the best prediction (c(ratio big gaps, ratio small gaps)) (see ?gapfillSSA for details )                                  }
  \item{amnt.iters}{list of integer vectors:
amount of iterations performed for the outer and inner
loop (c(outer,inner)) (see ?gapfillSSA for details)}
  \item{amnt.iters.start}{list of integer vectors:
index of the iteration to start with (outer, inner). If this
value is >1, the reconstruction (!) part is started with this iteration. Currently
it is only possible to set this to values >1 if amnt.artgaps and size.biggap == 0.}
  \item{calc.parallel}{logical:  whether to use parallel computing. Needs the packages doMC and foreach
to be installed.                                  }
  \item{debugging}{logical: if set to TRUE, debugging workspaces or dumpframes are saved at several stages
in case of an error.}
  \item{debugging.SSA}{
}
  \item{dimensions}{list of string vectors:
setting along which dimensions to perform SSA. These names
have to be identical to the names of the dimensions in the ncdf file. Set this to
'time' to do only temporal gap filling or to (for example) c('latitude','longitude')
to do 2 dimensional spatial gap filling. See the description for details on how to
perform spatio-temporal gap filling.}
  \item{file.name}{character: name of the ncdf file to decompose.  The file has to be in the current working directory!}
  \item{first.guess}{character string: if 'mean', standard SSA procedure is followed (using zero as the first guess).
Otherwise this is the file name of a ncdf file with the same dimensions
(with identical size!) as the file to fill which contains values used as a
first guess (for the first step of the process!). This name needs to be exactly
"<filename>_first_guess_step_1.nc".                                  }
  \item{force.all.dims}{logical: if this is set to true, results from dimensions not chosen as the best guess are used
to fill gaps that could not be filled by the best guess dimensions due to too gappy slices etc. .                                  }
  \item{gaps.cv}{numeric: ratio (between 0 and 1) of artificial gaps to be included prior to the first
cross validation loop that are used for cross validation.}
  \item{keep.steps}{logical: whether to keep the files with the results from the single steps}
  \item{M}{list of single integers: window length  or embedding dimension in time steps. If not
given,  a default value of 0.5*length(time series) is computed.                                                               }
  \item{max.cores}{integer: maximum number of cores to use (if calc.parallel = TRUE).}
  \item{max.steps}{integer: maximum amount of steps in the variances scheme}
  \item{n.comp}{list of single integers:
amount of eigentriples to extract (default (if no values are
supplied) is 2*amnt.iters[1] (see ?gapfillSSA for details)}
  \item{ocean.mask}{logical matrix: contains TRUE for every ocean grid cell and FALSE for land cells. Ocean grid
cells will be set to missing after spatial SSA and will be excluded from temporal SSA.
The matrix needs to have dimensions identical in order and size to the spatial dimensions
in the ncdf file. As an alternative to a R matrix, the name of a ncdf file can be supplied.
It should only contain one non coordinate variable with 1 at ocean cells and 0 at land cells.}
  \item{pad.series}{list of integer vectors (of size 2):
length of the extracts from series to use for
padding. Only possible in the one dimensional case. See the documentation of gapfillSSA for details!}
  \item{print.status}{logical: whether to print status information during the process}
  \item{process.cells}{character string:
which grid/series to process. 'gappy' means that only series grids with actual
gaps will be processed, 'all' would result in also non gappy grids to be subjected to SSA. The
first option results in faster computation times as reconstructions for non gappy grids/series
are technically not needed for gap filling, whereas the second option provides a better
understanding of the results trajectory to the final results.                                  }
  \item{process.type}{
}
  \item{ratio.const}{numeric: max ratio of the time series that is allowed to be above tresh.const for the time series
still to be not considered constant.                                 }
  \item{ratio.test}{numeric: ratio (0-1) of the data that should be used in the cross validation step. If set to 1,
all data is used.}
  \item{reproducible}{logical: Whether a seed based on the characters of the file name should be set
which forces all random steps, including the nutrlan SSA algorithm to be
exactly reproducible.}
  \item{size.biggap}{list of single integers:
length of the big artificial gaps (in time steps) (see ?gapfillSSA for details)}
  \item{tresh.const}{numeric: value below which abs(values) are assumed to be constant and excluded
from the decomposition.}
  \item{tresh.converged}{numeric: ratio (0-1): determines the amount of SSA iterations that have to converge so that no error
is produced.}
  \item{tresh.fill}{numeric fraction (0-1):
This value determines the fraction of valid values below which
series/grids will not be filled in this step and are filled with the first guess from the
previous step (if any). For filling global maps while using a ocean.mask you need
to set this value relative to the global grid size (and not only the land mask). Setting this
value to zero would mean that also slices/series without any "real" values are tried to
be filled with the "first guess" value of the last iteration alone. This can only be done
if the cross validation scheme is switched off (e.g. by setting amnt.artgaps and size.biggap
to zero.}
  \item{var.names}{character string: name of the variable to fill. If set to 'auto' (default), the name
is taken from the file as the variable with a different name than the dimensions. An
error is produced here in cases where more than one such variable exists.}
  \item{\dots}{further settings to be passed to gapfillSSA}
}
\details{This is a wrapper function to automatically load, gapfill and save a ncdf file using SSA.
Basically it automatically runs gapfillSSA (see also its documentation) for all                         
time series or grids in a ncdf file automatically. Theoretically and  
conceptionally all methods could also be applied to simple datacubes (i.e. R arrays) 
and not only ncdf files. However,  this has not yet been implemented. The values
for several function arguments have to be supplied as rather complicated nested lists                         
to facilitate the usage of different values per step (see 'stepwise calculation' for details)

dimensions:
It is generally possible to perform one,  two,  or spatiotemporal 3D SSA
(as in Buttlar et al (2014)) for gap filling.  This is set by using the argument
'dimensions'. If only one string corresponding to a dimension name in the target
ncdf file is supplied, only  vectors in the direction of this dimension are
extracted and filled. If two dimension names are supplied,
matrices  (i.e. spatial grids) along these dimension are extracted and 2D SSA
is computed. To start the 3D spatio - temporal scheme (Buttlar et.al (2014)) which
iterates between 1D temporal and 2D spatial SSA,  set
dimensions = list(list("time", c("longitude","latitude"))). 

stepwise calculation:
The algorithm can be run step wise with different settings for each step
where the results from each step can be used as 'first guesses' for the subsequent
step. To do this, amnt.artgaps, size.biggap, amnt.iters, n.comp, M, tresh.fill
and dimensions have to be supplied as lists. For each nth iteration step the
values of the corresponding nth list element will be used. At each of these
nth iteration steps, several repetitions with different dimensions are possible
(as is the case with the 3D spatiotemporal scheme). To facilitate this, the 
individual list elements at each step have to be lists containing the different
dimension names. As a consequence, all these arguments have to be nested lists.
This is the case also if only one dimension is used (i.e. to do only temporal
1D SSA,  dimensions = list(list('time')).
One example for an application where supplying different settings for all these
steps would be user defined spatio-temporal gap filling. This allows to clearly
define which dimension (and M, gap amount etc) to use at each step of the
process. On the contrary,  the spatiotemporal gapfilling method applied by
Buttlar et. al. (2014) uses identical settings for each (outer loop)
iteration step and automatically determines which dimension to use. For
this procedure the first list element of dimensions (and the other stepwise
arguments) is recycled during each step. Hence, a list of only length one has
to be supplied (dimensions = list(list(c('longitude','latitude'),'time')) )
(see details for dimensions above).


NCDF file specifications:
Due to limitations in the file size, the ncdf file has to contain one variable (and the dimensional
coordinate variables) (for the time being). This function will
create a second ncdf file identical to the input file but with an additional variable called 'flag.orig',
which contains zero for gapfilled values and 1 for not filled values.
The function has only been tested with ncdf files with two spatial dimensions (e.g. lat and long) and
one time dimension. Even though it was programmed to be more flexible, its functionality can not
be guaranteed under circumstances with more dimensions.                         

non fillable gid cells:
Using the 3D method would result in a completely filled datacube. To prevent
the filling of grid cells where no reasonable guess via the gapfilling may be
achieved (i.e. ocean grid cells in the case of terrestrial data), a matrix
indicating these grid cells can be supplied (see 'ocean.mask')
}
\value{Nothing is returned but a ncdf file with the results is written.
TODO remove aperm steps
TODO extract iloop convergence information for all loops
TODO test inner loop convergence scheme for scenarios 
TODO indicate fraction of gaps for each time series
TODO break down world into blocks
TODO integrate onlytime into one dimensional variances scheme
TODO facilitate one step filling process with global RMSE calculation
TODO save convergence information in ncdf files
TODO check for too gappy series at single dimension setting
TODO create possibility for non convergence and indicate this in results
TODO facilitate run without cross validation repetition
TODO test stuff with different dimension orders in the file and in settings
TODO substitute all length(processes)==2 tests with something more intuitive
TODO put understandable documentation to if clauses
TODO remove first guess stuff
TODO incorporate non convergence information in final datacube
TODO facilitate easy run of different settings (e.g. with different default settings)
TODO switch off "force.all.dims" in case of non necessity
TODO delete/modify MSSA stuff
obsolete MSSA stuff  
start parallel processing workers
insert gaps for cross validation      
determine different iteration control parameters
prepare parallel iteration parameters
determine call settings for SSA
get first guess
run calculation
TODO try to stop foreach loop at first error message!
test which dimension to be used for the next step
TODO whole step can be excluded for "one step" processes
determine first guess for next step
use first guess from other dimensions in case of too gappy series
exclude not to be filled slices (oceans etc)
save first guess data
TODO: add break criterium to get out of h loop
check what happens if gapfillSSA stops further iterations due to limiting groups of eigentriples
save process convergence information
save results 
delete first guess files}
\references{v. Buttlar, J., Zscheischler, J., and Mahecha, M. D. (2014): An extended approach for
spatiotemporal gapfilling: dealing with large and systematic gaps in geoscientific
datasets, Nonlin. Processes Geophys., 21, 203-215, doi:10.5194/npg-21-203-2014                       }
\author{Jannis v. Buttlar}



\seealso{\code{\link[Rssa]{ssa}}, \code{\link[spectral.methods]{gapfillSSA}}, \code{\link{decomposeNcdf}}}
\examples{
    ## prerequisites: go to dir with ncdf file and specify file.name
    file.name        = 'scen_3_0.5_small.nc'
    max.cores        = 8
    calc.parallel    = TRUE
    
    ##example settings for traditional one dimensional "onlytime" setting and
    ## one step
    amnt.artgaps     <- list(list(c(0.05, 0.05)));
    amnt.iters       <- list(list(c(3, 10)));
    dimensions       <- list(list("time")); 
    M                <- list(list(12)); 
    n.comp           <- list(list(6)); 
    size.biggap      <- list(list(5)); 
    var.name         <- "auto"
    process.type     <- "stepwise"
#    .gapfillNcdf(file.name = file.name, dimensions = dimensions, amnt.iters = amnt.iters, 
#                amnt.iters.start = amnt.iters.start, amnt.artgaps = amnt.artgaps, 
#                size.biggap = size.biggap, n.comp = n.comp, tresh.fill = tresh.fill,
#                M = M, process.type = process.type)
    
    
    
    ##example settings for 3 steps, stepwise and mono dimensional
    dimensions       = list(list('time'), list('time'), list('time'))
    amnt.iters       = list(list(c(1,5)), list(c(2,5)), list(c(3,5)))
    amnt.iters.start = list(list(c(1,1)), list(c(2,1)), list(c(3,1)))
    amnt.artgaps     = list(list(c(0,0)), list(c(0,0)), list(c(0,0)))
    size.biggap      = list(list(0),      list(0),      list(0))
    n.comp           = list(list(6),      list(6),      list(6))
    M                = list(list(12),     list(12),     list(12))
    process.type     = 'stepwise'
#    gapfillNcdf(file.name = file.name, dimensions = dimensions, amnt.iters = amnt.iters, 
#                amnt.iters.start = amnt.iters.start, amnt.artgaps = amnt.artgaps, 
#                size.biggap = size.biggap, n.comp = n.comp, tresh.fill = tresh.fill,
#                M = M, process.type = process.type)
    
    ##example settings for 4 steps, stepwise and alternating between temporal and spatial
    dimensions       = list(list('time'), list(c('longitude','latitude')),
      list('time'), list(c('longitude','latitude')))
    amnt.iters       = list(list(c(1,5)), list(c(1,5)), list(c(2,5)), list(c(2,5)))
    amnt.iters.start = list(list(c(1,1)), list(c(1,1)), list(c(2,1)), list(c(2,1)))
    amnt.artgaps     = list(list(c(0,0)), list(c(0,0)), list(c(0,0)), list(c(0,0)))
    size.biggap      = list(list(0),      list(0), list(0),      list(0))
    n.comp           = list(list(15),     list(15), list(15),     list(15))
    M                = list(list(23),     list(c(20,20)), list(23), list(c(20,20)))
    process.type     = 'stepwise'
#    gapfillNcdf(file.name = file.name, dimensions = dimensions, 
#                amnt.iters = amnt.iters, amnt.iters.start = amnt.iters.start, 
#                amnt.artgaps = amnt.artgaps, size.biggap = size.biggap, n.comp = n.comp, 
#                tresh.fill = tresh.fill, M = M, process.type = process.type, max.cores = max.cores)
    
    ##example setting for process with alternating dimensions but variance criterium
    dimensions       = list(list('time', c('longitude','latitude')))
    n.comp           = list(list(5,      5))
    M                = list(list(10,     c(10, 10)))
    amnt.artgaps     = list(list(c(0,0), c(0,0)))
    size.biggap      = list(list(0,      0))
    process.type     = 'variances'
    max.steps        = 2
#    gapfillNcdf(file.name = file.name, dimensions = dimensions, n.comp = n.comp, 
#                tresh.fill = tresh.fill, max.steps = max.steps, M = M, 
#                process.type = process.type, amnt.artgaps = amnt.artgaps, 
#                size.biggap = size.biggap, max.cores = max.cores)
}
