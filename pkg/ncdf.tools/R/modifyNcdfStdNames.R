modifyNcdfStdNames =  function(fileCon) {
  closeNcdf  <- FALSE
  if (inherits(fileCon, 'character')) {
    if (!file.exists(fileCon))
      stop('Specified file not existent!')
    fileCon <- open.nc(fileCon, write = TRUE)
    closeNcdf <-  TRUE
  }


  diminfo <- infoNcdfDims(fileCon)

  names.change =  list(latitude = c('lat'), longitude = c('lon', 'long'))
  for (i in 1:length(names.change)) {
    dim.change <-  diminfo[, 'name'][is.element(diminfo[, 'name'], names.change[[i]])]
     if(length(dim.change) == 1) {
       dim.rename.nc(fileCon, dim.change, names(names.change)[i])
       var.change <- infoNcdfVars(fileCon)[, 'name'][infoNcdfVars(fileCon)[, 'name'] == dim.change]
       if(length(var.change) == 1) {
         var.rename.nc(fileCon, var.change, names(names.change)[i])         
       }
     }
  }
  
  if (closeNcdf)
    close.nc(fileCon) 
}
