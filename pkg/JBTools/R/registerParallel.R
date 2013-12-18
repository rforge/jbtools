registerParallel <- function(
##title<< set up parallel computing front end
 pckg.parallel = 'doMC'  ##<< character string: package to use for parallel
                         ##   computing. Has to be one of doMC.
 , max.cores = 0         ##<< integer: amount of cores to use
)
  ##description<< This function automatically sets up the system so that
  ## parallel computing is possible for doMC (yet).
  ##seealso<< 
  ##\code{\link[foreach]{foreach}}, \code{\link[doMC]{registerDoMC}}
{
  
  if (max.cores == 0) 
    max.cores   <- getCoreLimit()
  if (max.cores == 1 || getDoParWorkers() < max.cores) {
    cat(paste('Registering ', max.cores, ' cores.\n', sep = ''))
    if (pckg.parallel == 'doMC'){
      w <<- max.cores
      registerDoMC(w)
    } else if (pckg.parallel == 'doParallel'){
      w <<- makeCluster(max.cores)
    } else if (pckg.parallel == 'snow') {
      stop('Do not use this function to create snow clusters. Use sfInit from package snowfall instead!')
    } else {
      stop(paste('Package ', pckg.parallel, ' is not (yet) supported!', sep=''))
    }
  }
}
