\name{readNcdfSpectral}
\alias{readNcdfSpectral}
\title{Read the results of a spectral decomposition (from a netCDF file)}
\description{readNcdfSpectral reads spectrally decomposed ncdf data (i.e. the output of a call to decomposeNcdf).}
\usage{readNcdfSpectral(fileName, varName, rangeBandsGet)}\arguments{
  \item{fileName}{character string: name of the netCDF file}
  \item{varName}{character string: name of the variable to extract.}
  \item{rangeBandsGet}{vector: Vector defining the bands to extract. Can be either
logical with one TRUE/FALSE per band in the file or a numeric
vector of length two with the lower and the upper spectral
border.}
}

\value{matrix: the spectral bands defined. }

\author{Jannis v. Buttlar}





