\name{indexDatacube}
\alias{indexDatacube}
\title{create logical index matrices for multidimensional datacubes}
\description{This function facilitates supplying logical index array for some 
of the dimensions of a data array. This mimics Matlabs indexing scheme.}
\usage{indexDatacube(datacube = c(),
 logical.ind, dims = "auto", dims.datacube = dim(datacube))}\arguments{
  \item{datacube}{array: datacube from which to extract the sub-parts
datacube and dims.datacube should be supplied.}
  \item{logical.ind}{logical array: TRUE/FALSE index matrix for a subset of the
dimensions of datacube. The size of logical.ind`s dimensions 
has to match the sizes of the corresponding dimensions in 
datacube.}
  \item{dims}{integer vector or 'auto' : indices of the dimensions in
datacube corresponding to the dimensions of logical.ind. 
If set to 'auto' this matching is tried to be accomplished 
by comparing the sizes of the dimensions of the two objects.}
  \item{dims.datacube}{integer vector: dimensions of datacube. Only one of
dims.datacube or datacube should be supplied!}
}

\value{integer index matrix which can be used to index datacube}

\author{Jannis v. Buttlar}





