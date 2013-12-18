userCoords <- structure(function(
##title<< transfer coordinates in plots to use relative coordinates
##description<< The fundtion transfers the coordinate system in any plot to a relative system
##               with values between 0 and 1. This helps to position text etc. in plots.
     x=c()    ##<< numeric vector(0-1): relative coordinates on the x axis
     ,y=c()   ##<< numeric vector(0-1): relative coordinates on the y axis
)
##details<< x and y need to be values between 0 and 1. These values are then mapped to the
##          coordinates used in the current plot.
{
    plot_extremes=par()$usr
    
    x_min=plot_extremes[1]
    x_max=plot_extremes[2]        
    y_min=plot_extremes[3]
    y_max=plot_extremes[4]
    if (par()$xlog) {
        x_trans=10^(x_min+(x*(x_max-x_min)))       
    } else {
        x_trans=x_min+(x*(x_max-x_min))        
    }        
    if (par()$ylog) {
        y_trans=10^(y_min+(y*(y_max-y_min)))
    } else {
        y_trans=y_min+(y*(y_max-y_min))
    }
    if (length(y)==0) {
        output=list(x=x_trans)
    } else if (length(x) ==0) {
        output=list(y=y_trans)
    } else {
        ##value<< list with x and/or y component with values in the current coordinate system        
        output=list(x=x_trans,y=y_trans)        
    }
    ##value<< list with x and y coordinates
    return(output)
},ex=function(){
    plot(1:10)
    text.coords <- userCoords(x=c(0.1,0.5),y=c(0.9,0.5))
    text(text.coords,labels=c('1st Text','2nd Text'))
})
