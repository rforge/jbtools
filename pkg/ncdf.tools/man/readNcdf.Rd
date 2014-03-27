\name{readNcdf}
\alias{readNcdf}
\title{fast reading of ncdf data}
\description{Convenience function to automatically read in data from a ncdf file
without specifying variable names and opening file connections.}
\usage{readNcdf(file.name, var.name = c())}\arguments{
  \item{file.name}{character string: name of the ncdf file file to read the data from.}
  \item{var.name}{character string: name of the variable to extract. If not supplied,
this is tried to be determined with readNcdfVarName().}
}

\value{(multidimensional) array: data from the ncdf file.}

\author{Jannis v. Buttlar}





