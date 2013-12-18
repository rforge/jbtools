getCoreLimit <- function()
  ##title<< get core limit
  ##description<<
  ## This function tries to get the amount of processors on a (Linux) machine.
{
  if (Sys.info()['sysname'] == 'Linux') {
    core.limit = as.integer(system('grep -c processor /proc/cpuinfo', intern = TRUE))
    if (exists('max.cores')) 
      core.limit = min(max.cores, core.limit)  
  } else {
    stop('Not possible (yet) on non linux systems.')
  }
  ##value<<
  ##integer: Amount of cores.
  return(core.limit)
}
