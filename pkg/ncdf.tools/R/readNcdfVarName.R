readNcdfVarName <- function(
  ##title<< get name of variable in ncdf file
  file ##<< connection to the ncdf file.
)


##description<<
## Try to automatically detect the name of the "main" variable in a ncdf file. The name returned is the
## name of the only non coordinate variable. If more than one of these is returned, the name of the variable
## having the most dimensions is used.

##seealso<<
## \code{\link[RNetCDF]{RNetCDF}}, \code{\link{infoNcdfVars}}

{
  if (class(file) == 'character') {
    file.con <- open.nc(file)
  } else {
    file.con <- file
  }  
  var.name         <- setdiff(infoNcdfVars(file.con, order.var ='id')$name, infoNcdfDims(file.con, extended = FALSE)$name)
  names.excluded   <- c('time_bnds')
  var.name         <- setdiff(var.name, names.excluded)
  var.name         <- var.name[!grepl('flag.orig$', var.name)]
  if(length(var.name) > 1) {
    var.id.nocoord <- infoNcdfVars(file.con, order.var ='id')[match(var.name, infoNcdfVars(file.con, order.var ='id')$name), 1]
    var.nocoord.ndims <- infoNcdfVars(file.con, order.var ='id')[var.id.nocoord + 1, 4]
    var.id <- var.id.nocoord[var.nocoord.ndims == max(var.nocoord.ndims)]    
    if (length(var.id) > 1 && class(file) == 'character') {
      names.nocoord <- infoNcdfVars(file.con, order.var = 'id')[var.id + 1,'name']
      var.id        <- var.id[which(!is.na(pmatch(names.nocoord, file)))]
    }
    if ((length(var.id) > 1) ) {
      stop('Not possible to detect variable name!')
    } else {
      var.name <-infoNcdfVars(file.con, order.var ='id')$name[var.id + 1]         
    }
  }
  if (class(file) == 'character') {
    close.nc(file.con)
  } 
  ##value<< character string: name of the variable.   
  return(var.name)
}  
