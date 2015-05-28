\name{plotGapfillSeries}
\alias{plotGapfillSeries}
\title{Plot an overview of a the results of a SSA gapfilling (from a ncdf file)}
\description{This function plots some overview statistics of the results of a gapfilling run
in a netCDF file,  i.e. the results of a call to gapfillNcdf().}
\usage{plotGapfillSeries(file.orig, file.filled = sub("[.]nc", "_gapfill.nc", 
    file.orig), data.orig = c(), data.filled = c(), ...)}
\arguments{
  \item{file.orig}{object to plot: file name or file.con object linking to a ncdf file}
  \item{file.filled}{character string: name of the
filled file.}
  \item{data.orig}{array: Unfilled data. Can be supplied to prevent loading
the data from a ncdf file again. This is read from
'file.filled' if no value is given.}
  \item{data.filled}{array: Filled data. Can be supplied to prevent loading
the data from a ncdf file again. This is read from
'file.filled' if no value is given.  }
  \item{\dots}{
}
}



\author{Jannis v. Buttlar}



\seealso{\code{\link{gapfillSSA}}, \code{\link{gapfillNcdf}}
\if{html}{\out{<img src="../doc/visualize_ncdf_demo.png" alt="image ..visualize_ncdf_demo should be here"/>}}\ifelse{latex}{}{}}

