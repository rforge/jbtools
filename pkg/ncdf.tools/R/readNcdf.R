readNcdf <- function(
  ##title<< fast reading of ncdf data
  file.name ##<< character string: name of the ncdf file file to read the data from.
  ,var.name = c()  ##<< character string: name of the variable to extract. If not supplied,
            ## this is tried to be determined with readNcdfVarName().
)
  ##description<<
  ## Convenience function to automatically read in data from a ncdf file
  ## without specifying variable names and opening file connections.
{
  file.con <- open.nc(file.name)
  if (length(var.name) == 0)
    var.name = readNcdfVarName(file.con)
  data     <- var.get.nc(file.con, var.name)
  close.nc(file.con)
  ##value<< (multidimensional) array: data from the ncdf file.
  return(data)
}  

