indexVec2Mat <- function(
  ... ##<< integer vectors: indices to use for the different dimensions
  ) {
  ##title<< Transform integer indices to an index matrix
  dummy=list(...)
  ##value<< Index matrix
  index.matrix <- as.matrix(do.call("expand.grid", list(...)))
  return(index.matrix)
}
