printStatus <- function(
##title<< print and save a status report
    string.in ##<< character string: status message to print
)
  ##description<< This function prints a given string as a status report to the screen together
  ## with the current time.
{
    cat(paste(Sys.time(),' : ',string.in,'\n',sep=''))
}
