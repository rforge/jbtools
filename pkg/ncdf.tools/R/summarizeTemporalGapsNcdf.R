summarizeTemporalGapsNcdf = function(
##title<< compute and plot statistics about temporal gaps in ncdf data cube
        ncdf.obj               ##<< file name or ncdf connection: link to the ncdf file to analyze
        , var.name = 'auto'    ##<< character string: name of the variable to analyze. If set to 'auto', 
                               ## the variable with the most dimensions will be used.
        , ind.groups = c()     ##<< integer vector: index to group the time steps into different groups
                               ## which are analyzed independently. This, for example, helps to 
                               ## see seasonal differences. Needs to have the same length as the time 
                               ## dimension
        , grids.exclude = NULL ##<< object to exclude grid cells (as, for example, ocean tiles) from the 
                               ## analysis. Can be a function that is applied to the vectors of the data cube 
                               ## which returns a single TRUE/FALSE value per vector indicating whether to use
                               ## the whole vector. Another possibility is to supply a list with elements longitude
                               ## and latitude consisting of vectors with the indices along the respective
                               ## dimension of grid cells to exclude. A last option is to supply a matrix of 
                               ## logical values with its dimensions being identical with the spatial 
                               ## dimensions in the ncdf file.
        , col.palette = colorRampPalette(c('blue','yellow','red'))  ##<< function: Color palette to use for the maps,
                               ## i.e. a function taking a single integer as an argument and returning
                               ## that many hexadecimal rgb values.
)
  ##description<<
  ## This function computes statistics like mean and maximum gap length for all time series in 
  ## the data cube of a ncdf file. 
  ##seealso<<
  ## \code{\link{summarizeTemporalGapsNcdf}}
  ##value<<
  ## Nothing is returned but plots of the gap statistics are produced.
{
    #test input
    if (class(ncdf.obj) == 'character' && !file.exists(ncdf.obj))
        stop('No file with the specified name exists')

    #prepare stuff
    if (class(ncdf.obj) == 'NetCDF') {
        file.con    <-  ncdf.obj
    } else {
        file.con    <-  open.nc(ncdf.obj)
    }
    if(var.name == 'auto')
        var.name    <-  infoNcdfVars(file.con)$name[which.max(infoNcdfVars(file.con)$n.dims)]
    if (!dim(infoNcdfDims(file.con))[1] == 3)
        stop('Only processing of three dimensional ncdf files is implemented!')
    if (sum(is.na(match(c('time','longitude','latitude'),infoNcdfDims(file.con)[,'name'])))>0)
        stop('ncdf file needs to have 3 dimensions called time, longitude and latitude!')
    
    #load data
    data            <-  var.get.nc(file.con, var.name) 
    dim.time        <-  match('time', infoNcdfDims(file.con)$name)
    dims.lengths    <-  infoNcdfDims(file.con)$length
    dims.names      <-  infoNcdfDims(file.con)$name    
    if (is.null(grids.exclude)) {
        grids.valid.matrix <-  array(TRUE,dim = dims.lengths[ - dim.time])
    } else if (is.function(grids.exclude)) {
        grids.valid.matrix <- !apply(data,MARGIN=(1:3)[ - dim.time], FUN = grids.exclude)
    } else if (is.list(grids.exclude)) {
        grids.valid.matrix <- array(TRUE,dim=dims.lengths[ - dim.time])
        grids.valid.matrix[cbind(grids.exclude[[dims.names[ - dim.time][1]]],grids.exclude[[dims.names[ - dim.time][2]]])] <-  FALSE
    } else if (is.array(grids.exclude) && length(dim(grids.exclude)) == 2) {
        if (!sum(dim(grids.exclude) == dims.lengths[ - dim.time]) == 2)
            stop('Size of the dimensions of grids.exclude does not match the size of the corresponding ncdf dimensions!')
        grids.valid.matrix <-  grids.exclude
    } else {
        stop('class of grids.exclude not suppoeted!')
    }
    
    #compute spatial maps
    dims.spatial.id <-  infoNcdfDims(file.con)$id[ - match('time'
                             , infoNcdfDims(file.con)$name)]   
    gap.stat.spatial<-  apply(data, MARGIN = dims.spatial.id + 1
                             , function(x){dummy=summarizeTemporalGaps(x);c(dummy$mean[1],dummy$max[1])})
    gap.stat.spatial[array(rep(!grids.valid.matrix,each=2),dim=c(2,dim(grids.valid.matrix)))]<- NA                 
    
    #compute global averages                 
    dims.order      <-  c(match('time', infoNcdfDims(file.con)$name), (1:3)[  -  dim.time])
    data.trans      <-  aperm(data, perm = dims.order)
    data.trans      <-  abind(data.trans, array(1, dim = c(1, dims.lengths[  -  dim.time]))
                        , along = 1)    
    data.clean      <-  data.trans[array(rep(as.vector(grids.valid.matrix)
                                  , each=dims.lengths[dim.time]+1),dim=dim(data.trans))]                                
    if (length(ind.groups) > 0){
        if (!((length(ind.groups) + 1) == dim(data.trans)[1]))
            stop('ind.groups needs to have the same length as the time dimension!')
        ind.groups.array  <-  array(ind.groups, dim = (dim(data.trans)  -  c(1, 0, 0)))
        ind.groups.trans  <-  abind(ind.groups.array
                        , array(1, dim = c(1, dims.lengths[  -  dim.time])), along = 1)    
        ind.groups.vector <-  ind.groups.trans[array(rep(as.vector(grids.valid.matrix)
                                , each=dims.lengths[dim.time]+1),dim=dim(data.trans))] 
    } else {
        ind.groups.vector <-  rep(1, times = prod(infoNcdfDims(file.con)$length))
    }
    gap.stat.global     <-  summarizeTemporalGaps(series = data.clean, index = ind.groups.vector
                , results.ext = TRUE)
    
    #plot stuff    
    par(mfrow=c(2,2), tcl=0.2, mgp=c(1, 0, 0), mar=c(1, 2, 1, 0),oma=c(1,0,2,0))     
    xmn <-  min(var.get.nc(file.con, 'longitude')) 
    xmx <-  max(var.get.nc(file.con, 'longitude')) 
    ymn <-  min(var.get.nc(file.con, 'latitude')) 
    ymx <-  max(var.get.nc(file.con, 'latitude')) 
    perm.grid <-  order(match(c('latitude', 'longitude'), dims.names))
    plot(raster(aperm(gap.stat.spatial[1, , ],perm=perm.grid), 
                    xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx), col = col.palette(30), main='mean gaplength')    
    plot(raster(aperm(gap.stat.spatial[2, , ],perm = perm.grid), 
                    xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx), col = col.palette(30), main='max gaplength')    
    

    y    <-  gap.stat.global$densities[[1]] * (1:length(gap.stat.global$densities[[1]])) / (length(data.clean) - prod(dims.lengths[dim.time])) * 100
    plot(y, type = 's',xlab='gap length [time steps]',ylab='part of series occupied by gap [%]', 
            main='global mean statistic')  
    if (length(ind.groups) > 0)
    {
        ylim <-  c(0.00001,max(unlist(gap.stat.global$densities[ - 1])))
        xlim <-  c(0,max(sapply(gap.stat.global$densities[ - 1],length)))  
        plot(1,1,xlim=xlim,ylim=ylim,type='n',ylab='amount of gaps',xlab='gap length [time steps]', 
                main='seasonal gaps')
        ind.seasons = 2:(length(unique(ind.groups.vector)) + 1)
        legend(x="topright", col=ind.seasons,legend=(paste('season ',ind.seasons - 1 ,sep='')),lty=1)
        for (i in ind.seasons)
        {   
            y = gap.stat.global$densities[[i]]
            x = (0:(length(y)+1)) - 0.5
            points(x,c(0,y,0), type = 's',col=i)
        } 
    }
    mtext(side = 3, outer = TRUE, line = 0, text = 'temporal gap statistic',cex=1.5)
    if (class(ncdf.obj) == 'NetCDF') {
        close.nc(file.con)
    }     
}
