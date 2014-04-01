\name{plotColorScale}
\alias{plotColorScale}
\title{Add a color scale to plots}
\description{plotColorScale is a wrapper function around color.legend to ease its usage.}
\usage{plotColorScale(col, zlim = c(), pos = list(x = c(1.02, 1.08), 
    y = c(0.1, 0.9)), align = "rb", gradient = "y", cex = 1, 
    cex.title = 1, title = "cts/px", outer.range = c(FALSE, FALSE), 
    legend = c())}\arguments{
  \item{col}{vector of color strings defining the palette to use}
  \item{zlim}{numeric vector (of length 2) defining the upper and lower limit
of the values mapped to the color scale.}
  \item{pos}{
}
  \item{align}{character: alignment option passed to color.legend}
  \item{gradient}{character: orientation option passed to color.legend}
  \item{cex}{numeric: character expansion factor for the text labels}
  \item{cex.title}{
}
  \item{title}{character: the title of the color scale}
  \item{outer.range}{logical: whether to extend the scale over the zlim borders
at its bottom and top.}
  \item{legend}{
}
}

\value{Nothing is returned.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{color.legend}}}

