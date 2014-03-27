\name{modifyNcdfCopyAtts}
\alias{modifyNcdfCopyAtts}
\title{copy all attributes between different variables}
\description{copies all attributes from one variable in a ncdf file to another variable (possibly in another file).}
\usage{modifyNcdfCopyAtts(file.con.orig,
 var.orig, var.copy, file.con.copy = file.con.orig)}\arguments{
  \item{file.con.orig}{a NetCDF object pointing to the original ncdf file from which to copy the attributes}
  \item{var.orig}{the name or id of the variable FROM which to copy all attributes}
  \item{var.copy}{the name or id of the variable TO which to copy all attributes}
  \item{file.con.copy}{a NetCDF object pointing to the ncdf file to which to copy the
attributes (same as file.con.orig by default)}
}

\value{nothing is returned.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{modifyNcdfCopyMetadata}}, \code{\link{modifyNcdfCopyVar}}}

