modifyNcdfStdNames =  function(
  ##title<< modify non standard longitude and latitude names
  fileCon ##<< file in which to modify the names. can be supplied as a character vector
          ##   or as a ncdf file connection.
  )
  ##description<< This function modifies lat, lon and long dimension names to 'latitude'
  ##              and 'longitude'
{
                                        # check for file and open if necessary
  closeNcdf  <- FALSE
  if (inherits(fileCon, 'character')) {
    if (!file.exists(fileCon))
      stop('Specified file not existent!')
    fileCon <- open.nc(fileCon, write = TRUE)
    closeNcdf <-  TRUE
  }
  
  # modify some latitude and longitude names
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

  #close ncdf file
  if (closeNcdf)
    close.nc(fileCon)
  ##value<< Nothing is returned
}
