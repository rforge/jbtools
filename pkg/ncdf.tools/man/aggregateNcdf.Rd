\name{aggregateNcdf}
\alias{aggregateNcdf}
\title{aggregate data in ncdf files}
\description{This function aggregates time periods in ncdf files. Basically it is just a
wrapper around the respective cdo function.}
\usage{aggregateNcdf(fileName, path.out = getwd(), period)}
\arguments{
  \item{fileName}{character vector: names of the files to aggregate.}
  \item{path.out}{character: path to save the results files to.}
  \item{period}{integer or one of hour, day, month or year: period to aggregate to. In case
of an integer value, the unit is time steps.}
}

\value{character string: name of the file created. }

\author{Jannis v. Buttlar}





