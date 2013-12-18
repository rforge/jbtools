isSeriesConstant <- function(
  ##title<< check whether series is constant
  x                     ##<< numeric vector: series to test.
  , tresh.const = 1e-12 ##<< numeric: maximum deviation allowed which is still
                        ##   considered to be constant.
  , ratio.const = 0.05  ##<< numeric: ratio of the series which is allowed to be
                        ##   not constant for the whole series to be still
                        ##   considered to be constant.
  )
  ##description<<
  ## This function checks whether a series is constant (up to a certain degree).

{
  if (sum(is.na(x)) == length(x)) {
    return(FALSE)
  } else {
    min.amount <-  (1 - ratio.const)*length(x[!is.na(x)])
    ##value<< logical: TRUE if series is constant, FALSE otherwise.
    return(sum(abs(x - median(x, na.rm = TRUE)) <  tresh.const, na.rm = TRUE) >= min.amount)
  }
}
