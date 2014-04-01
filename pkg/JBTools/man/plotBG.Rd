\name{plotBG}
\alias{plotBG}
\title{Plot a colored plot background}
\description{plotBG colors the plot background of the plot region (not the device region!).}
\usage{plotBG(color, exp.factor = 1, xlim = c(), ylim = c(), ...)}\arguments{
  \item{color}{color: which color to use}
  \item{exp.factor}{integer:possible expansion factor to use if very high values are plotted}
  \item{xlim}{
}
  \item{ylim}{
}
  \item{\dots}{further arguments passed to plot()}
}
\details{The function opens a plot with no content and plots a very large polygon of the given
color. Afterwards a new high level plot can be plotted over this background.}
\value{Nothing is returned.}

\author{Jannis v. Buttlar}





