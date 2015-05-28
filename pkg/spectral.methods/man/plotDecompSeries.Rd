\name{plotDecompSeries}
\alias{plotDecompSeries}
\title{Visualize/plot an overview of a SSA decomposed ncdf file.}
\description{This function plots an visualisation of a SSA decomposed ncdf file.  
\if{html}{\out{<img src="../doc/visualize_ncdf_demo.png" alt="image ..visualize_ncdf_demo should be here"/>}}\ifelse{latex}{}{}}
\usage{plotDecompSeries(file.orig, file.decomp = sub("[.]nc", "_specdecomp.nc", 
    file.orig), file.plot = "", ...)}
\arguments{
  \item{file.orig}{object to plot: file name or file.con object linking
to the original ncdf file}
  \item{file.decomp}{object to plot:
file name or file.con object linking to the decomposed ncdf file.}
  \item{file.plot}{character string: name of the file containing the
plots. If not given, a plot window is opened.}
  \item{\dots}{
}
}

\value{nothing is returned.}

\author{Jannis v. Buttlar}





