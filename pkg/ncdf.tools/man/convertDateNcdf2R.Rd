\name{convertDateNcdf2R}
\alias{convertDateNcdf2R}
\title{convert ncdf time vector to POSIXct}
\description{This function converts a time vector from a ncdf file or a vector of Julian days (or seconds, minutes, hours)
since a specified origin into a POSIXct R vector.}
\usage{convertDateNcdf2R(time.source, units = "days", origin = as.POSIXct("1582-10-14", 
    tz = "UTC"), time.format = c("\%Y-\%m-\%d", "\%Y-\%m-\%d \%H:\%M:\%S", 
    "\%Y-\%m-\%d \%H:\%M", "\%Y-\%m-\%d \%Z \%H:\%M", "\%Y-\%m-\%d \%Z \%H:\%M:\%S"))}
\arguments{
  \item{time.source}{numeric vector or ncdf connection: either a number of time units since
origin or a ncdf file connection, In the latter case, the time 
vector is extracted from the ncdf file, This file, and especially the 
time variable, has to follow the CF ncdf conventions.}
  \item{units}{character string: units of the time source. If the source
is a ncdf file, this value is ignored and is read from that file.}
  \item{origin}{POSIXct object:
Origin or day/hour zero of the time source. If the source
is a ncdf file, this value is ignored and is read from that file.}
  \item{time.format}{
}
}

\value{POSIXct vector: time vector in native R format}

\author{Jannis v. Buttlar}




