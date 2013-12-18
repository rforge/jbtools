colorChangeDarkness <- function(col, factor) {
  if (inherits(col, 'character')) 
    col = col2hex(col)
  
  ##TODO make loop easier
  n.cols.out <- max(c(length(col), length(factor)))
  col        <- rep(col, length.out = n.cols.out)
  factor     <- rep(factor, length.out = n.cols.out)
  cols.out   <- character(n.cols.out)
  for (i in 1:n.cols.out) {
    if (factor[i] < 1) {
      col.mix <- hex2RGB(col2hex('black'))
      alpha   <- 1 - (1- factor[i])
    }  else {
      col.mix <- hex2RGB(col2hex('white'))
      alpha   <- 1 - (factor[i] - 1)
    }
    cols.out[i] <- hex(mixcolor(alpha, col.mix, hex2RGB(col[i])))
  }
  return(cols.out)
}


