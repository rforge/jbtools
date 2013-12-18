plotAdditionalAxis <- function(
##title<< Add second axis with transferred labels
    side = 1      ##<< integer: which axis to use as a basis for the second one.
    ,trans.fun    ##<< function: the transfer function to use between the two axis values.
    ,label = c()  ##<< character: labels of the axis.
    ,...          ##<< further arguments to pass to the axis call.
    )
  ##description<<
  ## This function adds a second axis with differing labels to the plot.
  ##seealso<<
  ## \code{\link{axis}}
  ##author<<
  ## Jannis v. Buttlar, MPI BGC Jena, Germany, jbuttlar@bgc-jena.mpg.de
  ##value<<
  ## Nothing is returned.
{
    xaxis.values <- axTicks(side = (2 - side %% 2))
    side.new     <- (side + 1) %% 4 + 1
    axis(side = side.new, at = xaxis.values, labels = round(trans.fun(xaxis.values), digits = 2))
    mtext(side = side.new, text = label, line = 3)
}
