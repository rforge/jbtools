\name{checkInstalled}
\alias{checkInstalled}
\title{Check whether a command can be invoked via the command line}
\description{checkInstalled checks whether an external command can be run on the command line.}
\usage{checkInstalled(commandName)}
\arguments{
  \item{commandName}{character string: name of the program/command to check}
}
\details{The test is a simple wrapper around Sys.which which returns TRUE if which returns
a character string and FALSE if not. }
\value{logical: whether the program is installed.}

\author{Jannis v. Buttlar}





