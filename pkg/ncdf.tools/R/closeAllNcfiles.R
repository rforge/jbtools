closeAllNcfiles = function()
##title<< close all open ncdf connections
##description<< convenience function to close all ncdf connections that are 
##              currently open. 
{
  object.close=character(length=0)
  for (i in 1:length(ls(globalenv())))
    if (class(get(ls(globalenv())[i]))[1]=='NetCDF')
      {
        object.close=c(object.close,ls(globalenv())[i])
      }
  for (j in 1: length(object.close)) {
      try(close.nc(get(object.close[j],envir=globalenv())),silent=TRUE)
      rm(list=(object.close[j]),envir=globalenv())
    }
  ##value<< nothing is returned.
}
