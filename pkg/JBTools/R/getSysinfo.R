getSysinfo=function()
##title<< extract system information
##description<< convenience function to compile some information about the system used.
{
    package.names=sapply(sessionInfo()[['otherPkgs']],'[[','Package')
    package.versions=sapply(sessionInfo()[['otherPkgs']],'[[','Version')
    packages.all=paste(package.names,package.versions,collapse=', ',sep='')
    pars.sys=c('user','nodename','sysname','release')
    R.system=paste(sessionInfo()[[1]]$version.string)
    sys.info = paste(pars.sys,Sys.info()[pars.sys],collapse=', ',sep=':')
    all.info=paste(c(sys.info,', ',R.system,', installed Packages: ',packages.all),sep='',collapse='')
    ##value<< character string with all version and system information of the current R system
    return(all.info)
}
