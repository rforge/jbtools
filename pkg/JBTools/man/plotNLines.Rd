\name{plotNLines}
\alias{plotNLines}
\title{Plot many lines in one plot}
\description{plotNLines function uses different techniques to visualize many line plots on one display.}
\usage{plotNLines(x.data = matrix(1:dim(y.data)[2], ncol = dim(y.data)[2], 
    nrow = dim(y.data)[1], byrow = TRUE), y.data, option = c("normal", 
    "diff.scales", "stacked")[1], n.lines.max = 30, grid = FALSE, 
    scale = 1, plot.scale = TRUE, function.add = function() (1), 
    bgc = "white", pch.axlink = 1, type = "l", colors = c(), 
    xlim = c(), ylim = c(), xlab = c(), ylab = c(), labels = c(), 
    lty = 1, title, yaxt = "s", ...)}\arguments{
  \item{x.data}{
}
  \item{y.data}{numeric matrix/data frame: y-values with one series of
values per row}
  \item{option}{character: which type of plot to use (see details)}
  \item{n.lines.max}{integer: only for 'stacked plots': how many lines to draw per panel}
  \item{grid}{logical: only for 'stacked plots': whether to draw a primitive grid}
  \item{scale}{numeric: only for 'stacked plots': scale factor to scale the y scale of the stacked plots}
  \item{plot.scale}{TRUE: only for 'stacked plots': whether to add a small scale showing the y axis scale}
  \item{function.add}{function: only for 'stacked plots': function to call after plotting the individual panels}
  \item{bgc}{color: color of the plot background}
  \item{pch.axlink}{integer: only for 'stacked plots': pch value for the symbol that links each line to its x-axis}
  \item{type}{standard plotting parameter}
  \item{colors}{colors to use for the different series}
  \item{xlim}{standard plotting parameter}
  \item{ylim}{standard plotting parameter}
  \item{xlab}{standard plotting parameter}
  \item{ylab}{standard plotting parameter}
  \item{labels}{standard plotting parameter}
  \item{lty}{standard plotting parameter}
  \item{title}{standard plotting parameter}
  \item{yaxt}{standard plotting parameter}
  \item{\dots}{further arguments passed to the plot() calls}
}
\details{Many parameters are identical to standard plotting parameters (see ?par,?plot)
and are not explained here.
The function offers three options:
'normal':
plots all plots in one coordinate system colored according to colors
'diff scales':
plots all plots in the same region but uses different y axis scales. The visibility may
be limited to ~7 plots.
'stacked':
plots many plots in one region, all shifted vertically a bit to increase visibility. This allows
for the easy comparison of many similar plots (e.g. time series) but reduces the details that are
visible.
\if{html}{\out{<img src="../doc/plotNLines_demo.png" alt="image ..plotNLines_demo should be here"/>}}\ifelse{latex}{}{}}


\author{Jannis v. Buttlar}





