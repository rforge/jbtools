\name{plotGapfillSeries}
\alias{plotGapfillSeries}
\title{visualize/plot an overview of a SSA gapfilled ncdf file.}
\description{This function plots some overview statistics of a ncdf file.
\if{html}{\out{<img src="../doc/visualize_ncdf_demo.png" alt="image ..visualize_ncdf_demo should be here"/>}}\ifelse{latex}{}{}}
\usage{plotGapfillSeries(file.orig, file.filled = sub("[.]nc", "_gapfill.nc", 
    file.orig), data.orig = c(), data.filled = c(), ...)}
\arguments{
  \item{file.orig}{object to plot: file name or file.con object linking to a ncdf file}
  \item{file.filled}{character string: name of the
filled file.}
  \item{data.orig}{array: Unfilled data. Can be supplied to prevent loading
the data from a ncdf file again.    }
  \item{data.filled}{array: Filled data. Can be supplied to prevent loading
the data from a ncdf file again.   }
  \item{\dots}{
}
}



\author{Jannis v. Buttlar}





