\name{readNcdfSpectral}
\alias{readNcdfSpectral}
\title{read spectrally saved Ncdf data}
\description{Read spectrally saved ncdf data (i.e. the output of a call to decomposeNcdf).}
\usage{readNcdfSpectral(fileName, varName, rangeBandsGet)}\arguments{
  \item{fileName}{character string: name of the ncdf file}
  \item{varName}{character string: name of the variable to extract.}
  \item{rangeBandsGet}{vector: Vector defining the bands to extract. Can be either
logical with one TRUE/FALSE per band in the file or a numeric
vector of length two with the lower and the upper spectral
border.}
}

\value{matrix: the spectral bands defined. }

\author{Jannis v. Buttlar}





