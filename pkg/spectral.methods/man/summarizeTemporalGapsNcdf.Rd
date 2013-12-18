\name{summarizeTemporalGapsNcdf}
\alias{summarizeTemporalGapsNcdf}
\title{compute and plot statistics about temporal gaps in ncdf data cube}
\description{This function computes statistics like mean and maximum gap length for all time series in 
the data cube of a ncdf file. }
\usage{summarizeTemporalGapsNcdf(ncdf.obj, var.name = "auto", ind.groups = c(), 
    grids.exclude = NULL, col.palette = colorRampPalette(c("blue", 
        "yellow", "red")))}
\arguments{
  \item{ncdf.obj}{file name or ncdf connection: link to the ncdf file to analyze}
  \item{var.name}{character string: name of the variable to analyze. If set to 'auto',
the variable with the most dimensions will be used.}
  \item{ind.groups}{integer vector: index to group the time steps into different groups
which are analyzed independently. This, for example, helps to 
see seasonal differences. Needs to have the same length as the time 
dimension}
  \item{grids.exclude}{object to exclude grid cells (as, for example, ocean tiles) from the
analysis. Can be a function that is applied to the vectors of the data cube 
which returns a single TRUE/FALSE value per vector indicating whether to use
the whole vector. Another possibility is to supply a list with elements longitude
and latitude consisting of vectors with the indices along the respective
dimension of grid cells to exclude. A last option is to supply a matrix of 
logical values with its dimensions being identical with the spatial 
dimensions in the ncdf file.}
  \item{col.palette}{function: Color palette to use for the maps,
i.e. a function taking a single integer as an argument and returning
that many hexadecimal rgb values.}
}

\value{Nothing is returned but plots of the gap statistics are produced.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{summarizeTemporalGapsNcdf}}}

