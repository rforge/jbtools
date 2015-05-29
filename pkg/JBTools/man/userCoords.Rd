\name{userCoords}
\alias{userCoords}
\title{Transfer relative to actual plot coordinate values}
\description{userCoords transfers realtive coordinate values (i.e. values between
0 and 1) to the actual coordinate system of the plot.}
\usage{userCoords(x = c(), y = c())}
\arguments{
  \item{x}{numeric vector(0-1): relative coordinates on the x axis}
  \item{y}{numeric vector(0-1): relative coordinates on the y axis}
}
\details{x and y need to be values between 0 and 1. These values are then mapped to the
coordinates used in the current plot.}
\value{list with x and/or y component with values in the current coordinate system        }

\author{Jannis v. Buttlar}




\examples{
plot(1:10)
text.coords <- userCoords(x=c(0.1,0.5),y=c(0.9,0.5))
text(text.coords,labels=c('1st Text','2nd Text'))
}
