readNcdfCoordinates = function(
  ##title<< read coordinate or dimension values from ncdf file
  fileCon ##<< ncdf file connection or character string: Connection to the
          ##   ncdf file or its file name. In the latter case, the connection is
          ##   created and closed automatically.
  )
  ##description<<
  ##This function reads the coordinate values from a ncdf file.
  ##seealso<<
  ##\code{\link{infoNcdfDims}}
  {
    closeNcdf  <- FALSE
    if (inherits(fileCon, 'character')) {
      if (!file.exists(fileCon))
        stop('Specified file not existent!')
      fileCon <- open.nc(fileCon)
    }
    dimNames <- infoNcdfDims(fileCon)$name
    results <- list()
    for (dimNameT in dimNames) {
      if (is.element(dimNameT, infoNcdfVars(fileCon, dimvars = TRUE)$name)) {
        results[[dimNameT]] <- var.get.nc(fileCon, dimNameT)
      } else {
        results[[dimNameT]] <- NULL
      }
    }
    if (closeNcdf)
      close.nc(fileCon)
    ##value<< A list with the coordinate values (if available) for all dimensions.
    return(results)
  }
