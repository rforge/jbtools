rangeZeroEqui <- function(
  ##title<< compute a zero centered equi-sided range
  x ##<< input vector
  )
  ##description<< Compute a zero centered equi-sided range.
  {
  ##value<< vector
  return(c(-1,1)*max(abs(range(x, na.rm = TRUE))))
}

