\name{readNcdfVarName}
\alias{readNcdfVarName}
\title{get name of variable in ncdf file}
\description{Try to automatically detect the name of the "main" variable in a ncdf file. The name returned is the
name of the only non coordinate variable. If more than one of these is returned, the name of the variable
having using all dimensions or with a name appearing as a pattern in the file name is used.}
\usage{readNcdfVarName(file)}\arguments{
  \item{file}{connection to the ncdf file.}
}

\value{character string: name of the variable.   }

\author{Jannis v. Buttlar}



\seealso{\code{\link[RNetCDF]{RNetCDF}}, \code{\link{infoNcdfVars}}}

