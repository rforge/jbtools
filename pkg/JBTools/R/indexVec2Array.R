indexVec2Matrix = function(index, dim) {
  if (length(dim) == 2) {
    dim1 <- index - (floor((index - 1)/dim[1]) * dim[1]) 
    dim2 <- floor((index - 1)/dim[1]) + 1 
  } else {
    stop('Only 2d is yet implemented!')
  }
  ##value<< index array
  return(cbind(dim1, dim2))
}
