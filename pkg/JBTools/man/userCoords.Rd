\name{userCoords}
\alias{userCoords}
\title{transfer coordinates in plots to use relative coordinates}
\description{The fundtion transfers the coordinate system in any plot to a relative system
with values between 0 and 1. This helps to position text etc. in plots.}
\usage{userCoords(x = c(), y = c())}
\arguments{
  \item{x}{numeric vector(0-1): relative coordinates on the x axis}
  \item{y}{numeric vector(0-1): relative coordinates on the y axis}
}
\details{x and y need to be values between 0 and 1. These values are then mapped to the
coordinates used in the current plot.}
\value{list with x and/or y component with values in the current coordinate system        

list with x and y coordinates}

\author{Jannis v. Buttlar}




\examples{
plot(1:10)
text.coords <- userCoords(x=c(0.1,0.5),y=c(0.9,0.5))
text(text.coords,labels=c('1st Text','2nd Text'))
}
