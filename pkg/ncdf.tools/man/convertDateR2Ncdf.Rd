\name{convertDateR2Ncdf}
\alias{convertDateR2Ncdf}
\title{convert time vectors in ncdf files to Julian days since the start of Gregorian calendar   }
\description{This function automatically converts time vectors in ncdf files to a standardized Gregorian calendar}
\usage{convertDateR2Ncdf(ncdf.obj, date.vec = "auto", origin = "1800-01-01", 
    write.to.ncdf = TRUE)}\arguments{
  \item{ncdf.obj}{character string or ncdf connection: ncdf file for which to convert the dates}
  \item{date.vec}{POSIXct vector: date vectors for the time dimension. If set to 'auto', this
is tried to be extracted from the ncdf file}
  \item{origin}{character string: origin to be used for the time vector. This start of the
Gregorian calendar should be kept to avoid possible mistakes due to flawed
conversions.}
  \item{write.to.ncdf}{logical: whether to write the time vector to the ncdf file.}
}
\details{This function sets a time vector in a ncdf file to a standardized format which is readable by
most software. It transfers the time vector to days since the start of the Gregorian calendar.}
\value{(invisibly): the time vector. Additionally the time vector is written to the respective file.}

\author{Jannis v. Buttlar}





