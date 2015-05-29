\name{colorChangeDarkness}
\alias{colorChangeDarkness}
\title{Change the dark/brightness of a color}
\description{Function to yield darker or brighter variants of the same color
by mixing this color with white or black}
\usage{colorChangeDarkness(col, factor)}
\arguments{
  \item{col}{color to change. Can be a R color name or a hexadecimal color specification}
  \item{factor}{decimal factor to darken/brighten the color with. Values < 1 lead to darker
colors,  values > 1 to brighter colors.}
}

\value{hexadecimal code for the new color.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{mixcolor}}}

