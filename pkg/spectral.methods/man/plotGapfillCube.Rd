\name{plotGapfillCube}
\alias{plotGapfillCube}
\title{Plot an overview of a the results of a SSA gapfilling (from a ncdf file).}
\description{This function plots some overview statistics of the results of a gapfilling
(i.e. the results of a call to gapfillNcdf) .
\if{html}{\out{<img src="../doc/visualize_ncdf_demo.png" alt="image ..visualize_ncdf_demo should be here"/>}}\ifelse{latex}{}{}}
\usage{plotGapfillCube(file.orig, file.filled = sub("[.]nc", "_gapfill.nc", 
    file.orig), file.prefill = "", data.orig = c(), data.filled = c(), 
    data.prefill = c(), n.series = 16, lwd = 2, max.cores = 1, 
    ...)}
\arguments{
  \item{file.orig}{object to plot: file name or file.con object linking to the original (unfilled) ncdf file}
  \item{file.filled}{object to plot: file name or file.con object linking to the filled ncdf file}
  \item{file.prefill}{object to plot: file name or file.con object linking to the prefilled ncdf file}
  \item{data.orig}{array: data (see file.orig). Can be supplied to prevent
the reloading of huge datacubes.}
  \item{data.filled}{see data.orig}
  \item{data.prefill}{see data.orig}
  \item{n.series}{integer: how many example series to plot}
  \item{lwd}{graphical parameter, see ?par}
  \item{max.cores}{integer: amount of cores to use for parallelized computations.}
  \item{\dots}{
}
}

\value{some overview statistics of the different datacubes.}

\author{Jannis v. Buttlar}





