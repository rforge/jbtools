summarizeTemporalGaps <-  function(
  ##title<< compute statistics of temporal gaps in time series vectors
  series                 ##<< vector: input time series
  , index = rep(0, times = length(series)) ##<< index vector (optional): integer values indicating
                         ## different periods for which to compute the statistics independently.
  , results.ext = FALSE  ##<< logical: whether to compute and return detailed information like
                         ## sizes of individual gaps and count of a histogram.            
  )
  ##description<<
  ## This function computes the lengths of gaps in a vector and computes mean and max sizes and a histogram. 
{
  if (!length(series) == length(index))
    stop('series and index need to have identical lengths!')         
  results              <-  list()
  for (i in 1:(max(index) + 1)) {	
    series.t         <- numeric(length = 0)
    if (i == 1) {
      rle.obj.t    <-  rle(rep(TRUE, times = length(series)))
    } else {
      rle.obj.t    <-  rle(index == (i - 1))
    }	
    series.padgaps              <-  series
    last.pad                    <-  cumsum(rle.obj.t$lengths)[rle.obj.t$values] + 1
    first.pad                   <-  cumsum(rle.obj.t$lengths)[!rle.obj.t$values]
    ind.padgaps                 <-  sort(c(first.pad, last.pad))
    series.padgaps[ind.padgaps] <-  1
    if (i>1) {
      ind.extr                <-  index == (i - 1)
    } else {
      ind.extr                <-  rep(TRUE, times = length(series))
    }    
    ind.extr[ind.padgaps]       <-  TRUE
    series.t                    <-  series.padgaps[ind.extr]
    gaps.rle                    <-  rle(is.na(series.t))
    gaps.sizes                  <-  gaps.rle$lengths[gaps.rle$values]
    results$mean[[i]]           <-  mean(gaps.sizes)
    results$max [[i]]           <-  max(gaps.sizes)
    if (results.ext) {    
      results$densities[[i]]  <-  hist(gaps.sizes, breaks = 1:results[['max']][[i]], plot = FALSE)$counts
      results$lengths[[i]]    <-  gaps.sizes
    }    
  }
  results ##values<< list of elements: mean and maximum sizes of gaps, amount of gaps with identical sizes (densities) and
          ##         lengths of all gaps. Each of these list elements may contain several values. The first is always the global 
          ##         mean, the following the statistics for values grouped according to index.
}
