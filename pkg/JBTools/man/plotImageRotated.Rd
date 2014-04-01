\name{plotImageRotated}
\alias{plotImageRotated}
\title{Plot a rotated image plot }
\description{plotImageRotated plots a raster/matrix using image() but supplying
a rotated version of the matrix,  i.e. it plots the matrix as it
printed on the screen.         }
\usage{plotImageRotated(data, col.vals = c(), row.vals = c(), scale = TRUE, 
    col = heat.colors(20), zlim = range(data, na.rm = TRUE), 
    title = "", useRaster = TRUE, xlab = "", ylab = "", ...)}\arguments{
  \item{data}{matrix: data to be plotted}
  \item{col.vals}{numeric vector: coordinate values for the columns of data}
  \item{row.vals}{numeric vector: coordinate values for the rows of data}
  \item{scale}{logical: whether to plot a color scale besides the plot}
  \item{col}{color vector: colors to create the color scale.}
  \item{zlim}{numeric vector (length two): outer
limits of the color-scale. Values above or below this 
range are colored brighter/darker than the maximum/minimum 
color (see ?plotColorScale).}
  \item{title}{character string: title of the color legend}
  \item{useRaster}{argument passed to image() to decide whether to
draw polygons (FALSE) or use a bitmap raster (TRUE).}
  \item{xlab}{
}
  \item{ylab}{
}
  \item{\dots}{further arguments passed to image}
}
\details{The normal image() function always plots a rotated version of a matrix. This function 
rotates the input to image in a way that the first entry of the matrix (col=1, row=1)
shows up at the top-left corner of the plot. In other words the way a matrix would be 
displayed by printing it directly mapped to the plot.}
\value{Nothing is returned.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{image}}, \code{\link{plotColorScale}}, the plotting routines of the raster package}

