\name{convertFilename2Date}
\alias{convertFilename2Date}
\title{convert file name to R date}
\description{This function converts parts of ncdf file names to date strings. This is used, e.g.
in transNcdfCutFiles.   }
\usage{convertFilename2Date(file.names, fun.extr.string, fun.conv.string)}\arguments{
  \item{file.names}{character vector: names of the files}
  \item{fun.extr.string}{function}
  \item{fun.conv.string}{function}
}

\value{POSIXct object with the date.}

\author{Jannis v. Buttlar}



\seealso{\code{\link{transNcdfCutFiles}}  }

