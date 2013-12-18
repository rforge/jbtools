modifyNcdfCopyAtts <- function(
##title<< copy all attributes between different variables
##description<< copies all attributes from one variable in a ncdf file to another variable (possibly in another file).
   file.con.orig                   ##<< a NetCDF object pointing to the original ncdf file from which to copy the attributes
   , var.orig                      ##<< the name or id of the variable FROM which to copy all attributes
   , var.copy                      ##<< the name or id of the variable TO which to copy all attributes
   , file.con.copy = file.con.orig ##<< a NetCDF object pointing to the ncdf file to which to copy the
                                   ##   attributes (same as file.con.orig by default)
   )
##seealso<<
##\code{\link{modifyNcdfCopyMetadata}}, \code{\link{modifyNcdfCopyVar}}
{
    n.atts <- var.inq.nc(file.con.orig, var.orig)$natts
    if (n.atts > 0) {
        for (i in 1:n.atts)  {
            att.name  <- att.inq.nc(file.con.orig, var.orig, i - 1)$name
            att.type  <- att.inq.nc(file.con.orig, var.orig, i - 1)$type
            att.value <- att.get.nc(file.con.orig, var.orig, att.name)
            att.put.nc(file.con.copy, var.copy, att.name, att.type, att.value)
        }
    }
    ##value<< nothing is returned.
}
