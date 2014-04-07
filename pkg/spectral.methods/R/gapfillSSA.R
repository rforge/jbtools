gapfillSSA <- structure(function(
##title<< Fill gaps in a vector (time-series) with SSA
    amnt.artgaps = c(0.05, 0.05)     ##<< numeric vector: The relative ratio (amount gaps/series length) of
                                     ##   artificial gaps to include to determine the iteration with the best
                                     ##   prediction (c(ratio big gaps, ratio small gaps)). If this is set to
                                     ##   c(0,0), the cross validation step is excluded and the iteration
                                     ##   is run until amnt.iters.
    , DetBestIter = '.getBestIteration'##<< function: Function to  determine the best outer and inner iteration to
                                     ##   use for reconstruction. If no function is given, the standard way is used.
                                     ##   (see ?.getBestIteration)
    , debugging = FALSE              ##<< logical: If set to TRUE, workspaces to be used for debugging are
                                     ##   saved in case of (some) errors or warnings.
    , amnt.iters = c(10, 10)         ##<< integer vector: Amount of iterations performed for the outer and inner
                                     ##   loop (c(outer,inner)).
    , amnt.iters.start = c(1, 1)     ##<< integer vector: Index of the iteration to start with c(outer, inner). If this
                                     ##   value is > 1, the reconstruction (!) part is started with this iteration. Currently
                                     ##   it is only possible to set this to values > 1 if amnt.artgaps != 0 as this would
                                     ##   cause a cross validation loop. 
    , fill.margins = FALSE           ##<< logical: Whether to fill gaps at the outer margins of the series, i.e. to
                                     ##   extrapolate before the first and after the last valid value. Doing this most
                                     ##   probably produces unreliable results (i.e. a strong build up of amplitude).
    , first.guess = c()              ##<< numeric vector/matrix: First guess for the gap values. The mean/zero is used if
                                     ##   no value is supplied. Has to have the same dimensions and lengths as series.
    , GroupEigTrpls = 'clusterify'   ##<< character string: Name of the function used to group the eigentriples. This function
                                     ##   needs to take a ssa object as its first input and other inputs as its ... argument.
                                     ##   It has to return a list with the length of the desired amount of SSA groups. Each of
                                     ##   its elements has to be a integer vector indicating which SSA eigentriple(s) belong(s)
                                     ##   to this group. Possible settings are 'clusterify' or 'groupSSANearestNeighbour'. In the case of
                                     ##   'clusterify' this is automatically set to 'groupSSANearestNeighbour' in the case of 2d-ssa.
    , kind = c('auto', '1d-ssa', '2d-ssa')[1]   ##<< character string: Whether to calculate one or two dimensional SSA (see the
                                     ##   help of ssa()). Default is to determine this automatically by determining
                                     ##   the dimensions of series.
    , M = floor(length(series) / 3)  ##<< integer: Window length  or embedding dimension [time steps]. If not
                                     ##   given,  a default value of 0.33*length(timeseries) is computed. For
                                     ##   2d SSA a vector of length 2 has to be supplied. If only one number is given,
                                     ##   this is taken for both dimensions. (see ?ssa, here the parameter is called L)
    , matrix.best.iter = 'perf.all.gaps'##<< character string: Which performance matrix to use (has to be one of
                                     ##   recstr_perf_a, recstr_perf_s or recstr_perf_b (see ?.getBestIteration)).
    , MeasPerf = 'RMSE'              ##<< character string: Name of a function to determine the 'goodness of fit'
                                     ##   between the reconstruction and the actual values in the artificial
                                     ##   gaps. The respective function has to take two vectors as an input and
                                     ##   return one single value. Set to the "Residual Mean Square Error" (RMSE) by default.  
    , n.comp = 2 * amnt.iters[1]     ##<< integer: Amount of eigentriples to extract (default if no values are
                                     ##   supplied is 2*amnt.iters[1]) (see ?ssa, here the parameter is called neig).
    , open.plot = TRUE               ##<< logical: Whether to open a new layout of plots for the performance plots. 
    , plot.results = FALSE           ##<< logical: Whether to plot performance visualization for artificial gaps?
    , plot.progress = FALSE          ##<< logical: whether to visualize the iterative estimation of the reconstruction process
                                     ##   during the calculations.
    , pad.series = c(0, 0)           ##<< integer vector (length 2): Length of the part of the series to use for
                                     ##   padding at the start (first value) and at the end of the series. Values
                                     ##   of zero cause no padding. This feature has not yet been rigorously tested!
    , print.stat = TRUE              ##<< logical: Whether to print status information during the calculations.   
    , remove.infinite = FALSE        ##<< logical: Whether to remove infinite values prior to the calculation.  
    , scale.recstr = TRUE            ##<< logical: whether to scale the reconstruction to sd = 1 at the end of each outer
                                     ##   loop step.
    , series                         ##<< numeric vector/matrix: equally spaced input time series or matrix with gaps (gap = NA)
    , seed = integer()               ##<< integer: Seed to be taken for the randomized determination of the positions of the
                                     ##   artificial gaps and the nutrlan ssa algorithm. Per default, no seed is set.
    , size.biggap = 20               ##<< integer: Length of the big artificial gaps (in time steps)  
    , SSA.methods = c('nutrlan', 'propack', 'eigen', 'svd')  ##<< character vector: Methods to use for the
                                     ##   SSA computation. First the first method is tried, when convergence
                                     ##   fails the second is used and so on. See the help of ssa() in
                                     ##   package Rssa for details on the methods. The last two methods
                                     ##   are relatively slow!
    , tresh.convergence = 0.01       ##<< numeric value: Threshold below which the last three sums of squared differences between
                                     ##   inner iteration loops must fall for the whole process to be considered to have converged.
    , tresh.min.length  = 5          ##<< integer: minimum length the series has to have to do computations.   
    , z.trans.series = TRUE          ##<< logical: whether to perform z-transformation of the series prior to
                                     ##   the calculation.  
)
##description<<
## gapfillSSA applies the iterative gap filling procedure proposed by Kondrashov and Ghil
## (2006) in a fast and optimized way developed by Korobeneykov
## (2009). Generally spoken, major periodic components of the time series are
## determined and interpolated into gap positions. An iterative cross validation
## scheme with artificial gaps is used to determine these periodic components.
                        
##details<<
## Artificial Gaps:
## The amount of artificial gaps to be included is determined as follows:
## amnt.artgaps determines the total size of the artificial gaps to be
## included.  The number (0-1) determines the number a relative ratio of the
## total amount of available datapoints. To switch off the inclusion of either
## small or biggaps, set respective ratio to 0.  In general the ratios determine
## a maximum amount of gaps.  size.biggap sets the size of the
## biggaps. Subsequently the number of biggaps to be included is determined by
## calculating the maximum possible amount of gaps of this size to reach the
## amount of biggaps set by amnt.artgaps[1]. The amount of small gaps is then
## set according to the ratio of amnt.artgaps[1]/amnt.artgaps[2].
##
## Iteration performance measure:
## The DetBestIter function should take any of the RMSE matrices (small/big/all gaps)
## as an input and return i_best with best inner loops for each outer loop and h_best
## as the outer loop until which should be iterated. Use the default function as a
## reference.
##
## Visualize results:
## If plot_per == TRUE an image plot is produced visualizing the RMSE between
## the artificial gaps and the reconstruction for each iteration. A red dot
## indicates the iteration chosen for the final reconstruction.
##
## Padding:
## For padding the series should start and end exactly at the start and end of a
## major oscillation (e.g. a yearly cycle and the length to use for padding
## should be a integer multiple of this length. The padding is solved internally
## by adding the indicated part of the series at the start and at the end of the
## series. This padded series is only used internally and only the part of the
## series with original data is returned in the results.  Padding is not (yet)
## possible for two dimensional SSA.
##
## Multidimensional SSA:
## 1d or 2d SSA is possible. If a vector is given, one dimensional SSA is
## computed. In case of a matrix as input, two dimensional SSA is performed. For
## the two dimensional case two embedding should be given (one in the direction
## of each dimension). If 'big gaps' are set to be used for the cross
## validation, quadratic blocks of gaps with the size
## 'size.biggap'*'size.biggap' are inserted.

##references<<
## Kondrashov, D. & Ghil, M. (2006), Spatio-temporal filling of missing points in geophysical data sets,
## Nonlinear Processes In Geophysics,S 2006, Vol. 13(2), pp. 151-159
## Korobeneykov, A. (2009), Computation- and Space-Efficient Implementation of SSA, ArXiv e-prints,
## www.adsabs.harvard.edu/abs/2009arXiv0911.4498K

##keyword<<
## SSA, gap filling, time series, spectral analysis, singular spectrum analysis

##seealso<<
## \code{\link[Rssa]{ssa}}

###########################    Preparations      ####################################
{
  #save input arguments
  args.check          <- as.list(environment())
  
  ## load libraries
  if (print.stat)
    printStatus('gap filling: Performing data preparation...')
  if (kind == '2d-ssa') {
    plot.progress <- FALSE
    SSA.methods   <- SSA.methods[is.na(match(SSA.methods, c('eigen', 'svd')))]
  }
  
  ## save settings
  settings            <- list(z.trans.series = z.trans.series, M = M, n.comp = n.comp,
                             amnt.artgaps = amnt.artgaps, size.biggap = size.biggap,
                             pad.series = pad.series, matrix.best.iter = matrix.best.iter,
                             SSA.method = 'none')
  settings$char.string <- paste(paste(names(settings), sapply(settings, function(x)paste(x, collapse = ',  '))
                                      , sep = ': '), collapse = '; ')
  
  ## check input arguments
  if (sum(is.na(series)) == 0) {
    print('Series does not contain any gaps. Gap filling makes no sense!')
    return(list(filled.series = series, reconstr = rep(NA, length(series)), settings =settings))
  }


  results.argscheck <- do.call(.gapfillSSACheckInput, args.check)
  kind              <- results.argscheck$kind
  GroupEigTrpls     <- results.argscheck$GroupEigTrpls
  n.dims            <-results.argscheck$n.dims
  n.rows            <- results.argscheck$n.rows
  series            <- results.argscheck$series
  M                 <- results.argscheck$M
  kind              <- results.argscheck$kind 
  SSA.methods       <- results.argscheck$SSA.methods
  n.comp            <- results.argscheck$n.comp
  amnt.artgaps      <- results.argscheck$amnt.artgaps
  error_occoured    <- FALSE
  colors            <- colorRampPalette(c('blue', 'red'))(amnt.iters[2])

  ## pad series
  results.padseries <- .gapfillSSAPadSeries(pad.series = pad.series, n.dims = n.dims,
                                            series = series, first.guess = first.guess,
                                            fill.margins = fill.margins)
  ind.padded  <- results.padseries$ind.padded
  series.work <- results.padseries$series.work
  first.guess <- results.padseries$first.guess
  ind.valid   <- results.padseries$ind.valid
  n.cols      <- results.padseries$n.cols

  ## modify M and n.comp
  if (kind == '2d-ssa' && length(M) == 1)
    rep(M, times = 2)
  if (kind == '1d-ssa' && M >= length(series.work) / 1.5) {
    M <- floor(length(series.work)/1.5)
    n.comp            <- min(M, length(series.work) - M + 1, n.comp)
  }
  
  ## Prepare variables
  n.datapts        <- length(series.work)
  ind_plot_progress<- sort(sample(1:n.datapts, min(c(1000, n.datapts))))
  perf.all.gaps    <- matrix(nrow = amnt.iters[1], ncol = amnt.iters[2])  # matrix for prediction performance in all gaps
  dimnames(perf.all.gaps) <- list(paste('os_', 1:amnt.iters[1], sep = ''),
                                  paste('is_', 1:amnt.iters[2], sep = ''))
  perf.big.gaps    <- matrix(nrow = amnt.iters[1], ncol = amnt.iters[2])  # matrix for prediction performance in big gaps
    dimnames(perf.big.gaps) <- list(paste('os_', 1:amnt.iters[1], sep = ''),
                                  paste('is_', 1:amnt.iters[2], sep = ''))
  perf.small.gaps    <- matrix(nrow = amnt.iters[1], ncol = amnt.iters[2])  # matrix for prediction performance in small gaps
  dimnames(perf.small.gaps) <- list(paste('os_', 1:amnt.iters[1], sep = ''),
                                  paste('is_', 1:amnt.iters[2], sep = ''))
  recstr_diffsum   <- array(NA, dim = c(amnt.iters[1], amnt.iters[2]-1, 2))  # matrix for prediction performance in small gaps
  dimnames(recstr_diffsum ) <- list(paste('os_',  1:(amnt.iters[1]), sep = ''),
                                  paste('is_',   1:(amnt.iters[2]-1), '-', 2:(amnt.iters[2]), sep = ''),
                                    c('cv', 'final'))
  iloop_converged  <- vector(mode='logical', length = amnt.iters[1])
  iloop_converged  <- array(FALSE, dim=c(amnt.iters[1], 2))
  dimnames(iloop_converged) <- list(c(paste('outer_step_', 1:amnt.iters[1], sep = '')),
                             c('crossvalidation', 'filling'))
  break.outer.loop <- FALSE
  i_best           <- array(NA, dim=c(amnt.iters[1], 2))
  dimnames(i_best) <- list(c(paste('outer_step_', 1:amnt.iters[1], sep = '')),
                             c('crossvalidation', 'filling'))

  ## prepare output list
  ##value<< list with components
  results <- list(
    error_occoured = error_occoured  ##<< logical: whether a non caught error occoured in one
                                       ##   of the SSA calculations.
    , filled.series = series           ##<< numeric vector/matrix: filled series with the same
                                       ## length as series but without gaps. Gaps at the margins
                                       ## of the series can not be filled and will occur in
                                       ## filled.series (and reconstr).
    , i_best = i_best                  ##<< integer matrix: inner loop iteration for each outer loop step in
                                       ## which the process has finally converged (depending on the
                                       ## threshold determined by tresh.convergence). If the RMSE
                                       ## between two inner loop iterations has been monotonously
                                       ## sinking (and hence, the differences between SSA iterations
                                       ## can be expected to be rather small), this is set to amnt.iters[2].
                                       ## If not, the process most likely has been building up itself, this
                                       ## is set to 0. In both cases iloop_converged is set FALSE.
    , iloop_converged =  iloop_converged##<< logical matrix: Whether each outer loop iteration has converged
                                       ## (see also i_best).
    , iter.chosen = c(NA, NA)          ##<< integer vector: iterations finally chosen for the
                                       ## reconstruction.   
    , perf.all.gaps = perf.all.gaps    ##<< numeric matrix: performance (RMSE) for the filling
                                       ## of all artificial gaps.
    , perf.small.gaps = perf.small.gaps##<< numeric matrix: performance (RMSE) for the filling
                                       ## of the small artificial gaps.
    , perf.big.gaps = perf.big.gaps    ##<<  numeric matrix: performance (RMSE) for the filling
                                       ## of the big artificial gaps.
    , process_converged = FALSE        ##<< logical: Whether the whole process has converged. For
                                       ## simplicity reasons, this only detects whether the last outer loop
                                       ## of the final filling process has converged.
    , reconstr = rep(NA, length(series))##<< numeric vector/matrix: filtered series or reconstruction
                                       ## finally used to fill gaps.
    , recstr_diffsum = recstr_diffsum  ##<< numeric matrix: RMSE between two consecutive inner loop iterations.
                                       ## This value is checked to be below tresh.convergence to determine
                                       ## whether the process has converged.                                
    , settings = settings              ##<< list: settings used to perform the calculation.
    , variances = rep(NA, n.comp)
    )
  ##end<<
  
  ## Center series around 0 and scale to stdev=1 (if wanted)
  results.centering  <- .gapfillSSACenterSeries(series.work = series.work, 
                                               z.trans.series = z.trans.series,
                                               first.guess = first.guess)
  mean_pars <- results.centering$mean_pars; series.work <- results.centering$series.work; 
  rescale_std <- results.centering$rescale_std; series.untouched <- results.centering$series.untouched

  ## insert artificial big gaps (if wanted)
  results.insrtgaps  <- .gapfillSSAInsertGaps(amnt.artgaps = amnt.artgaps, n.datapts = n.datapts, 
                                             n.dims = n.dims, size.biggap = size.biggap, 
                                             series = series, series.work = series.work,
                                             seed = seed, kind = kind, ind.padded = ind.padded,
                                             series.untouched = series.untouched)
  index_biggap <- results.insrtgaps$index_biggap; index_artgaps <- results.insrtgaps$index_artgaps; 
  index_allgaps <- results.insrtgaps$index_allgaps; index_smallgaps <- results.insrtgaps$index_smallgaps; 
  series.work <- results.insrtgaps$series.work; index_gaps <- results.insrtgaps$index_gaps
  
  ## stop in case too few datapoints are available
  if (length(series.work) < tresh.min.length | isSeriesConstant(series.work)) {
    if (length(series.work) < tresh.min.length)
      print('tresh.min.length exceeds (padded etc) series length. Returning input series.')
    if (isSeriesConstant(series.work))
      print('Series with artificial gaps inserted is constant. Returning input series.')
    if (debugging) {
      str.time <- gsub('[[:space:]]', '-', gsub('[[:punct:]]', '-', as.character(Sys.time())))
      path.debug  <- file.path('/Net', 'Groups', 'BGI', 'tmp','jbuttlar', 
                               'Cluster_jobs_debugging', sub('/Net/Groups/BGI/', '', getwd()))
      if (!file.exists(path.debug))
        system(paste('mkdir -p ', path.debug, sep = ''))     
      file.name.debug  <- paste(path.debug, '/debug_begin_', str.time, sep = '')            
      dump.frames(dumpto = file.name.debug, to.file = TRUE)
      print(paste('Saved debugging workspace to file ', file.name.debug, '.rda', sep = ''))
    }
    return(results)  
  }

#############################     Cross validation Loop     ################################
  ## fill all gaps with mean  = 0
  if (length(first.guess) == 0) {
    series.work[index_allgaps] <- 0
  } else {
    series.work[index_allgaps] <- (first.guess[index_allgaps] - mean_pars) / rescale_std
    if (sum(is.na(first.guess[index_allgaps])) > 0)
      series.work[index_allgaps[is.na(first.guess[index_allgaps])]] <- 0
  }
  
  ## perform iterative SSA loop
  if (sum(amnt.artgaps > 0) > 0) {
    if (print.stat)
      printStatus('gap filling: Performing SSA iteration...')
    for (h in 1:amnt.iters[1]) {
      series.loopstart <- series.work

      if (plot.progress) {                         
        dev.new()
        yrange <- range(series.work[ind_plot_progress]) + c(-1,1)* diff(range(series.work[ind_plot_progress]))
        plot((1:n.datapts)[ind_plot_progress], series.work[ind_plot_progress],
             col = 'gray', type ='b', ylim = yrange)
        points((1:n.datapts)[setdiff(ind_plot_progress, ind.padded)],
               series.work[setdiff(ind_plot_progress, ind.padded)], col = 'black')
      }     
      for (i in 1:amnt.iters[2]) {
        if (sum(is.na(series.work)) > 0) 
          stop('Series to fill still contains NAs,  which is most probably caused by code error!')
        if (i == amnt.iters.start[2]) {
          run.grouping = TRUE
          ssa.groups.t = list()
        } else {
          run.grouping = FALSE
        }            
        args.ssa <- list(series.in = series.work, M = M, n.comp = n.comp, 
                         kind = kind, GroupEigTrpls =  GroupEigTrpls, 
                         iterindex = paste(h, '/', i, sep = ''), 
                         SSA.methods = SSA.methods, ssa.groups.t = ssa.groups.t, 
                         seed = seed + (h * amnt.iters[2] + i), 
                         run.grouping = run.grouping, debugging = debugging)                                                                 
        results.ssa  <- do.call(.calcSSAAllMethods, args.ssa)   
        if (results.ssa[['error']]) {
          results$error_occoured <- TRUE
          return(results)          
        }  
        ssa.groups.t <- results.ssa[['ssa.groups.t']]
        max.recstr           <- min(c(h, length(ssa.groups.t)))            
        rcstr_t              <- colSums(matrix(unlist(results.ssa[['recstr.res']][1:max.recstr]),
                                               nrow = max.recstr, ncol = n.datapts, byrow = TRUE))
        perf.small.gaps[h, i]<- do.call(MeasPerf, list(series.untouched[index_smallgaps],
                                                       rcstr_t[index_smallgaps]))
        perf.big.gaps[h, i]  <- do.call(MeasPerf, list(series.untouched[index_biggap],
                                                       rcstr_t[index_biggap]))
        perf.all.gaps[h, i]  <- do.call(MeasPerf, list(series.untouched[index_artgaps],
                                                       rcstr_t[index_artgaps]))
        series.work[index_allgaps] <- rcstr_t[index_allgaps]
        if (i > 1) {
          recstr_diffsum[h, i-1, 1] <- sqrt(mean((rcstr_t - recstr.last)^2))
          if ((i > 3) && (sum(recstr_diffsum[h, (i-4):(i-1), 1] > tresh.convergence) == 0)) {
            i_best[h, 1] <- i
            break
          }
        }
        recstr.last <- rcstr_t    
        if (plot.progress) {
          points((1:n.datapts)[],   rcstr_t , col = colors[i], type = 'l')
          Sys.sleep(0.02)
        }
      }
      if (i == amnt.iters[2]) {                                                  # if not converged
        i_best[h, 1] <- amnt.iters[2]
        if (recstr_diffsum[h, i-1, 1] > recstr_diffsum[h, 1, 1]) {
          series.work  <- series.loopstart
          i_best[h, 1] <- 0
        }
      } else {                                                                   # if converged
        if (scale.recstr) {
          series.work[index_allgaps]  = (rcstr_t/sd(rcstr_t))[index_allgaps]
        } else {
          series.work[index_allgaps] <- rcstr_t[index_allgaps]              
        }                
        if (h+1 > length(ssa.groups.t) & amnt.iters[1] > length(ssa.groups.t)) {
        print(paste('Extraction of ', h + 1, ' groups of eigentriples not possible!',
                    ' Stopping outer iteration after step ', h, '!',sep = ''))
        break.outer.loop = TRUE
        if (debugging) {
          str.time <- gsub('[[:space:]]', '-', gsub('[[:punct:]]', '-', as.character(Sys.time())))
          path.debug  <- file.path('/Net', 'Groups', 'BGI', 'tmp','jbuttlar', 
                                   'Cluster_jobs_debugging', sub('/Net/Groups/BGI/', '', getwd()))
          if (!file.exists(path.debug))
            system(paste('mkdir -p ', path.debug, sep = ''))     
          file.name.debug  <- paste(path.debug, '/debug_convergence_', str.time, sep = '')            
          dump.frames(dumpto = file.name.debug, to.file = TRUE)
          printStatus(paste('Saved debugging workspace to file ', file.name.debug, '.rda', sep = ''))
        }
        break
      }
      } 
      if (print.stat) {
        printStatus(paste('gap filling: Finished ', h, ' of ', amnt.iters[1],
                            ' outer iterations.', sep = ''))
      }
    }
    h_best      <- do.call(DetBestIter, list(get(matrix.best.iter)))$h_best
    iter.chosen <- c(h_best, mean(i_best[1:h_best, 1], na.rm = TRUE))
  } else {
    h_best      <- amnt.iters[1]
    i_best[, 1] <- rep(amnt.iters[2], each =  amnt.iters[1])
  }

##########################    final Reconstruction        #########################
  if (print.stat)
    printStatus('gap filling: Reconstructing timeseries...')
  series.work                   <- series.untouched
  if (length(first.guess) == 0) {
    series.work[index_gaps]   <- 0
  } else {
    series.work[index_gaps]   <-  (first.guess[index_gaps] - mean_pars) / rescale_std
    if (sum(is.na(first.guess[index_gaps]))>0)
      series.work[index_gaps[is.na(first.guess[index_gaps])]] <- 0
  } 
  for (h in amnt.iters.start[1]:h_best) {
    series.loopstart <- series.work
    for (i in amnt.iters.start[2]:amnt.iters[2]) {            
      if (i == amnt.iters.start[2]) {
        run.grouping = TRUE
        ssa.groups.t = list()
      } else {
        run.grouping = FALSE
      }            
      args.ssa    <- list(series.in = series.work, M = M, n.comp = n.comp, 
        kind = kind, GroupEigTrpls =  GroupEigTrpls, 
        iterindex = paste(h, '/', i, sep = ''), 
        SSA.methods = SSA.methods, ssa.groups.t = ssa.groups.t, 
        seed = seed + (h * amnt.iters[2] + i), 
        run.grouping = run.grouping, debugging = debugging)
      results.ssa <- do.call(.calcSSAAllMethods, args.ssa)   
      ssa.groups.t<- results.ssa[['ssa.groups.t']]            
      if (results.ssa[['error']]) {
        results$error_occoured <- TRUE
        return(results)          
      }
      n.comps     <- min(c(h, length(ssa.groups.t)))
      recstr.t    <- colSums(matrix(unlist(results.ssa[['recstr.res']][1:n.comps]),
                                   nrow = n.comps, ncol = n.datapts, byrow = TRUE))
      series.work[index_allgaps]  <- recstr.t[index_allgaps] 
      if (i > 1) {
        recstr_diffsum[h, i-1, 2] <- sqrt(mean((recstr.t - recstr.last)^2))
        if ((i > 3) && (sum(recstr_diffsum[h, (i-4):(i-1), 2] > tresh.convergence) == 0)) {
          i_best[h, 2] <- i
          break
        }
      }
      recstr.last <- recstr.t    
    }
    if (i == amnt.iters[2]) {                                                   ## if not converged
      i_best[h, 2]<-  amnt.iters[2]
      if(recstr_diffsum[h, i-1, 2] > recstr_diffsum[h, 1, 2]) {
        series.work  <- series.loopstart
        i_best[h, 2] <- 0
      }
    } else {                                                                    ## if converged
      if (scale.recstr) {  
      series.work[index_allgaps]  = (recstr.t /sd(recstr.t))[index_allgaps]
    }   
      if (h + 1 > length(ssa.groups.t) & amnt.iters[1] > length(ssa.groups.t)) {
      print(paste('Extraction of ', h + 1, ' groups of eigentriples not possible!',
                  ' Stopping outer iteration after step ', h, '!',sep = ''))
      if (debugging) {
        str.time <- gsub('[[:space:]]', '-', gsub('[[:punct:]]', '-', as.character(Sys.time())))
        path.debug <- file.path('/Net', 'Groups', 'BGI', 'tmp', 'jbuttlar', 
                                'Cluster_jobs_debugging', sub('/Net/Groups/BGI/', '', getwd()))
        if (!file.exists(path.debug))
          system(paste('mkdir -p ', path.debug, sep = ''))     
        file.name.debug  <- paste(path.debug, '/debug_convergence_', str.time, sep = '')            
        dump.frames(dumpto = file.name.debug, to.file = TRUE)
        printStatus(paste('Saved debugging workspace to file ', getwd(), '/',
                            file.name.debug, '.rda', sep = ''))
      }
      break.outer.loop = TRUE
      break
    }
    }
    if (print.stat) {
      printStatus(paste('Reconstruction: finished ', h, ' of ', h_best,
                          ' outer iterations.', sep = ''))
    }
  }
  if (sum(amnt.artgaps > 0) == 0)
    iter.chosen <- c(h_best, mean(i_best[1:h_best, 2], na.rm = TRUE))

  if (print.stat)
    printStatus('Reconstruction: Iteration process finished.')
  
############################   Reconstruct Series    ##############################
  series.out             <- series.untouched    
  recstr                 <- matrix(recstr.t, nrow = n.rows, ncol = n.cols)

  if (scale.recstr) {
    recstr               <- recstr/sd(as.vector(recstr))
  }
  if (n.dims == 1)
    recstr               <- as.vector(recstr)
  series.out[index_gaps] <- recstr[index_gaps]
  series.out             <- (series.out * rescale_std)  + mean_pars
  recstr                 <- (recstr * rescale_std) + mean_pars
  
  if (sum(is.na(series.out)) > 0)
    stop('Filled series still contains gaps! Consider reprogramming the function!')
  
############################   prepare output        ##############################
  
  
  ## remove padding and extend to excluded margins
  if (!length(ind.padded) == 0) {  # if series was padded
    series.out <- series.out[ - ind.padded]
    recstr     <- recstr[ - ind.padded]
  } else if (!fill.margins && diff(ind.valid) != (length(series) - 1)) {  # if empty margins were excluded prior to filling
    series.out  <- c(rep(NA, ind.valid[1]-1), series.out, rep(NA, length(series) - ind.valid[2]))
    recstr      <- c(rep(NA, ind.valid[1]-1), recstr, rep(NA, length(series) - ind.valid[2]))
  }

  ## make estimate whether process converged
  iloop_converged = i_best < amnt.iters[2]
  iloop_converged[is.na(iloop_converged)] <- FALSE
  results$process_converged = iloop_converged[iter.chosen[1], 2]
  if (!results$process_converged & print.stat)
    cat('Iteration process seems not to have converged! Check recstr_diffsum for hints!')

  ##prepare results for return
  results$settings$SSA.method = results.ssa[['method.used']]
  results$filled.series = series.out  
  results$i_best = i_best  
  results$iloop_converged =  iloop_converged
  results$iter.chosen = iter.chosen 
  results$perf.all.gaps = perf.all.gaps 
  results$perf.small.gaps = perf.small.gaps
  results$perf.big.gaps = perf.big.gaps
  results$reconstr = recstr
  results$recstr_diffsum = recstr_diffsum                    
  results$variances = (results.ssa[['ssa.res']]$sigma[1:n.comp] / sum(results.ssa[['ssa.res']]$sigma[1:n.comp]))

  ## plot cross validation performance
  if (plot.results)
    .gapfillSSAPlot(open.plot = open.plot, results = results, amnt.iters = amnt.iters,
                   iter.chosen = iter.chosen, i_best = i_best, MeasPerf = MeasPerf)
  return(results)
}, ex = function(){
  ## create series with gaps
  series.ex <- sin(2 * pi * 1:1000 / 100) +  0.7 * sin(2 * pi * 1:1000 / 10) +
    rnorm(n = 1000, sd = 0.4)
  series.ex[sample(c(1:1000), 30)] <- NA
  series.ex[c(seq(from = sample(c(1:1000), 1), length.out = 20),
              seq(from = sample(c(1:1000), 1), length.out = 20))]<-NA
  indices.gaps <- is.na(series.ex)

  ## prepare graphics
  layout(matrix(c(1:5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7), ncol = 5, byrow = TRUE), 
         widths = c(1, 1, 1, 0.1, 0.1))
  par(mar = c(2, 0, 0, 0.2), oma = c(0, 3, 2, 0.2), tcl = 0.2, mgp = c(0, 0, 100),
      las = 1)

  ## perform gap filling
  data.filled <- gapfillSSA(series = series.ex, plot.results = TRUE, open.plot = FALSE)

  ## plot series and filled series
  plot(series.ex, xlab = '', pch = 16)
  plot(data.filled$filled.series, col = indices.gaps+1, xlab = '', pch = 16)
  points(data.filled$reconstr, type = 'l', col = 'blue')
  mtext(side = 1, 'Index', line = 2)
  legend(x = 'topright', merge = TRUE, pch = c(16, 16, NA), lty = c(NA, NA, 1), 
         col = c('black', 'red', 'blue'),
         legend = c('original values', 'gap filled values', 'reconstruction'))
})


#################################      check input     ###########################################
.gapfillSSACheckInput <- function(amnt.artgaps,  amnt.iters, amnt.iters.start, debugging,
                                 DetBestIter, first.guess, fill.margins, GroupEigTrpls, kind,
                                 matrix.best.iter, MeasPerf, n.comp, M, open.plot,
                                 pad.series, print.stat, plot.results, plot.progress,
                                 remove.infinite , scale.recstr, seed,
                                 series, size.biggap, SSA.methods, tresh.convergence,
                                 tresh.min.length, z.trans.series)
{
  ##ToDO do something useful when no gaps are in the data


  ## check dimensionality
  if (!is.element(class(series),c('numeric','matrix','array')))
    stop('class of series is not supported!')
  if (class(series) == 'numeric' | ((class(series) == 'array' | class(series) == 'matrix') & 
             length(dim(series)) == 1)) {
    n.cols <- length(series)      
    n.dims <- 1
    n.rows <- 1
    series <- as.vector(series)
    dim.series <- length(series)
  } else if (length(dim(series)) == 2) {
    n.dims <- 2
    n.cols     <- dim(series)[2]
    n.rows     <- dim(series)[1]
    dim.series <- dim(series)
    if (GroupEigTrpls == 'clusterify')
      GroupEigTrpls = 'groupSSANearestNeighbour'
    if (prod(M) < 10) {
      stop(paste('SSA for such small window sizes (~ < sqrt(10)) is only possible for methods which',
                 ' are not (yet) implemented for the 2d case!'))
    }
  } else {
    stop('More than 2 dimensional ssa is not suppported!')
  }
  if (kind == 'auto')
    kind <- paste(n.dims, 'd-ssa', sep='')
  if (n.dims > 1 & kind == '1d-ssa')
    stop('series is at least two dimensional but ssa is set to be calculated one dimensionally!')
  
  ## check whether amnt.artgaps is too small
  if(sum(amnt.artgaps != 0) > 0) {
    n.series.valid <- diff(range(which(!is.na(series))))
    n_biggaps   <- max(c(floor(n.series.valid * amnt.artgaps[1] / size.biggap^(n.dims)), 1))  
    if (amnt.artgaps[1] > 0) {
      n_smallgaps <- n_biggaps * size.biggap^(n.dims) * amnt.artgaps[1] / amnt.artgaps[2]
    } else {
      n_smallgaps <- n.series.valid * amnt.artgaps[2]
    }
    n.gaps <- n_smallgaps + n_biggaps*size.biggap
    if (sum(!is.na(series)) < 1.2* n.gaps)
      stop('Specified artificial gap sizes would be too large!')
  } 

  ## Parameter plausibility checks
  if (!(length(amnt.artgaps)==2))
    stop('Please supply two values for amnt.artgaps (i.e. values for small and big gaps)!')
  if (sum(amnt.artgaps == 0) == 2 & !(size.biggap  ==  0 & !plot.results))
    stop(paste('Inconsistent input! If amnt.artgaps=c(0, 0), then ',
                   'size.biggap == 0 and plot.results=FALSE!', sep=''))
  if (amnt.artgaps[1] == 0 & !(size.biggap == 0))
    stop(paste('Inconsistent input! If amnt.artgaps[1] == 0, then ',
               'size.biggap == 0!', sep=''))
      if (sum(amnt.artgaps) > 1)
        stop('Inconsistent input! sum(amnt.artgaps) needs to be <1!')
  if (remove.infinite)
    series[is.infinite(series)] <- NA
      if (sum(is.infinite(series)) > 0)
        stop('Time series includes Inf values. Remove them or set remove.infinite == TRUE')
  if (length(unique(na.omit(as.vector(c(series, first.guess))))) == 1) {
    stop('Series contains only constant values. Spectral gap filling makes no sense!')
    if (sum(!is.na(series)) == 0) {
      if (length(first.guess) == 0 | sum(!is.na(first.guess))==0)
        stop('series (and first guess) contain only missing values. gap filling not possible!')
      if (!all(amnt.artgaps == 0) | !(size.biggap == 0))
        stop(paste('If series contains only NA, cross validation is not possible ',
                   '(e.g. amnt.artgaps and size.biggap need to be set to zero!', sep=''))
    }
  }
  if (missing(series))
    stop('Time series need to be supplied!')
  if (sum((prod(M)) / n.comp < 2) > 0 && (is.element('nutrlan', SSA.methods) | is.element('propack', SSA.methods))) {
    if (print.stat)
      print(paste('n.comp exceeds (M^n.dims)/2! As using nutrlan in such cases is not appropriate,  ',
                  'only the remaining methods are used (1d case) or n.comp is adapted (2d)!', sep=''))
    if (n.dims == 1) {
      SSA.methods <- SSA.methods[ - match(c('nutrlan', 'propack'), SSA.methods)]
      if (length(SSA.methods) == 0)
        stop('No SSA methods left. Specify other methods or adjust n.comp to M!')
    } else {
      n.comp = floor(prod(M - 1) / 2)
    }
  }
  if (sum(n.comp > (prod(M))) > 0)
    stop('Amount of eigentriples to extract exceeds window length!')
  if (sum(dim.series < M) > 0)
    stop('Window length exceeds series length!')
  if (n.dims == 1 && (sum(c(n.cols, n.rows)[1:n.dims] < (n.comp+M)) > 0))
    stop('M + n.comp exceeds series length!')
  
  if(!is.numeric(pad.series) || !(length(pad.series) == 2) || sum(pad.series < 0) > 0)
    stop('pad.series has to be a positive integer vector of length 2!')
  if(sum(pad.series > length(series)^(1/n.dims)) > 0)
    stop('pad.series must not exceed the length of the timeries!')
  test.vec <- c(size.biggap, amnt.iters, n.comp, M, pad.series, amnt.iters.start)
  if(sum(!(abs(test.vec-round(test.vec)) < .Machine$double.eps^0.5)) > 0)
    stop('All values of size.biggap, amnt.iters, n.comp, M and pad.series have to be integer values!')
  if (n.dims > 1 & sum(pad.series > 0) > 0)
    stop('Padding of two dimensional matrices not yet implemented!')
  if (length(first.guess) > 1) {
    if (n.dims == 1) {
      if (!length(series) == length(first.guess))
        stop('Series and first guess have to have identical dimensions!')
    } else {
      if (!sum(dim(series) == dim(first.guess)) == n.dims)
        stop('Series and first guess have to have identical dimensions!')
    }
  }
  if (!length(amnt.iters)==length(amnt.iters.start))
    stop('amnt.iters.start has to be of the same length as amnt.iters!')
  if (sum(amnt.iters.start>amnt.iters)>0)
    stop('all values in amnt.iters.start have to be smaller than the respective ones in amnt.iters!')
  if (sum(amnt.artgaps !=0 ) > 0 & sum(amnt.iters.start != 1) > 0 )
    stop(paste('Supplying values of amnt.iters.start>1 is only reasonable if no cross validation',
               ' is used (e.g if amnt.artgaps==0)!',sep=''))
  if (amnt.iters.start[2] > 1)
    stop(paste('Do not (for now) supply values for amnt.iters.start[2] >1 as this ',
               'may lead to unintended and not tested side effects!', sep=''))
  return(list(kind = kind, GroupEigTrpls = GroupEigTrpls, n.dims = n.dims,
              n.rows = n.rows, series = series, M = M, kind = kind, SSA.methods = SSA.methods,
              n.comp = n.comp, amnt.artgaps = amnt.artgaps))
}



#################################       pad series       ###########################################
.gapfillSSAPadSeries <- function(pad.series, n.dims, series, first.guess, fill.margins) {
  ## create padding
  if (n.dims == 1) { # if one d SSA                                  
    if (sum(pad.series > 0) > 0) {  # if padding should be performed
      ind.padded     <- c(1:pad.series[1], (pad.series[1] + length(series)) + 1:pad.series[2])
      series.work    <- c(series[1:pad.series[1]], series, series[-(1:(length(series) - pad.series[2]))])
      if (length(first.guess) > 0)                
        first.guess <- c(first.guess[0:pad.series[1]], first.guess,
                         first.guess[-(1:(length(series)^(1 / n.dims) - pad.series[2]))])                       
    } else { # if no padding should be performed
      ind.padded     <- integer(0)
      series.work    <- series
    }
    
    ## remove gappy outer margins
    ind.valid   <- range(which(!is.na(series.work)))
    if (!fill.margins) {
      series.work <- series.work[ind.valid[1]:ind.valid[2]]
      if (length(first.guess) > 0)                
        first.guess <- first.guess[ind.valid[1]:ind.valid[2]]      
      ind.padded  <- ind.padded - ind.valid[1] + 1
      ind.padded  <- ind.padded[ind.padded > 0 & (ind.padded <= ind.valid[2] - ind.valid[1] + 1)]     
    }
    n.cols = length(series.work)


  ## set default values for 2d ssa (padding and margin exclusion is not implemented here)  
  } else { # if 2d SSA
    ind.padded <- integer(0)
    n.cols     <- dim(series)[2]
    series.work<- series
    ind.valid  <- c(1, length(series)) 
  }
  return(list(ind.padded = ind.padded, series.work = series.work, first.guess = first.guess,
              ind.valid = ind.valid, n.cols = n.cols))
}



#################################         ztransform series          ###########################################

.gapfillSSACenterSeries <- function(series.work, z.trans.series, first.guess)
{
    mean_pars 	     <- mean(series.work[!is.na(series.work)])    # mean of timeseries
    series.work            <- (series.work - mean_pars)
    if (z.trans.series) {
        rescale_std          <- sd(as.vector(series.work), na.rm = TRUE)  # rescaling factor (=stdev(series))
        series.work          <- (series.work) / rescale_std
    } else {
        rescale_std          <- 1
    }
    series.untouched       <- series.work
    if (all(is.na(series.work)) & length(first.guess) > 0) {
        mean_pars          <- mean(as.vector(first.guess), na.rm = TRUE)
        rescale_std        <- sd(as.vector(first.guess), na.rm = TRUE)
    }
    return(list(mean_pars = mean_pars, series.work = series.work, rescale_std = rescale_std
                , series.untouched = series.untouched))
}


#################################         insert artificial gaps     ###########################################

.gapfillSSAInsertGaps <- function(amnt.artgaps, n.datapts, n.dims, size.biggap, series, seed
                                 , series.work, kind, ind.padded, series.untouched) {
    n_biggaps        <- 0                                              # amount of big gaps (changed later if biggaps are to be included)
    index_biggap     <- integer()                                      # index of positions in series where to include biggaps
    n_smallgaps      <- 0                                              # amount of small gaps (changed later if smallgaps are to be included)
    index_smallgaps  <- integer()                                      # index of positions in series where to include smallgaps
    index_gaps       <- which(is.na(series.work))
    
    ## TODO think about big gaps for MSSA
    ## insert big gaps
    if (amnt.artgaps[1] > 0)  {
        n_biggaps   <- max(c(floor(n.datapts * amnt.artgaps[1] / size.biggap^(n.dims)), 1))
        if (kind == '2d-ssa')
        {
            mar.window      <- round(size.biggap * 0.2, digits=0)
            series.dummy    <- series
            series.dummy[ - (size.biggap:(dim(series)[1] - size.biggap)), ]      <- NA
            series.dummy[, - (size.biggap:(dim(series)[2] - size.biggap))]      <- NA
            w <-  size.biggap + 2 * mar.window + ifelse((size.biggap + 2 * mar.window)%%2, 0, -1)            
            movg.averg      <- t(array(focal(raster(series.dummy), w = w ,
                                               fun = function(x, ...){sum(!is.na(x))}, na.rm = FALSE)[], dim = dim(series.dummy)[2:1]))
            ind.biggaps.cp  <- matrix(NA, ncol = 2, nrow = n_biggaps)
        }
        series.dummy    <- series.work
        series.dummy[ind.padded] <- NA
        series.dummy[c(1:size.biggap, (length(series.work) - size.biggap) : length(series.work))] <- NA
        
        for (l in 1:n_biggaps) {
            if (kind == '1d-ssa') {
                if (length(seed)>0)
                  set.seed((seed*l)%%(2^31) + 1)
                rle.ob      <- rle(!is.na(series.dummy))
                rle.valid   <- (1:length(rle.ob$lengths))[rle.ob$values]
                ind.biggest <- which(rle.ob$values)[which.is.max(rle.ob$lengths[rle.ob$values])]
                ind.sequence<- 1 : (rle.ob$lengths[ind.biggest]) + sum(rle.ob$lengths[0 : (ind.biggest - 1)])
                index_biggap_t <- 0 : (size.biggap - 1) + sample(rep(ind.sequence[ 1 : max(c(min(c(length(ind.sequence),
                            (length(ind.sequence) - size.biggap)  )),1 )) ], each = 2), 1)
            } else {
                ind.highest          <- which(movg.averg == max(movg.averg, na.rm = TRUE), arr.ind = TRUE)
                ind.biggaps.cp[l, ]  <- ind.highest[sample(1:dim(ind.highest)[1], 1), ]
                ind.row.gap          <- seq(-1 * floor(size.biggap / 2), floor(size.biggap / 2)) + ind.biggaps.cp[l, 1]
                ind.col.gap          <- seq(-1 * floor(size.biggap / 2), floor(size.biggap / 2)) + ind.biggaps.cp[l, 2]
                movg.averg[ind.row.gap, ind.col.gap]  = 0
                series.work[ind.row.gap, ind.col.gap] = NA
                index_biggap_t       <- sort(rep(ind.row.gap, each = length(ind.col.gap)) +
                                             rep((ind.col.gap-1), times = length(ind.row.gap)) * dim(series)[1])
            }
            index_biggap   <- c(index_biggap, index_biggap_t)
            series.dummy[index_biggap_t] <- NA
        }
        index_biggap              <- sort(index_biggap)
        gaps_in_biggap            <- sum(is.na(series.untouched[index_biggap]))
        index_biggap              <- index_biggap[!is.na(series.untouched[index_biggap])]
        series.work[index_biggap] <- NA
    }

    ## insert artificial small gaps (if wanted)
    if (amnt.artgaps[2] > 0) {
        if (amnt.artgaps[1] > 0) {
            n_smallgaps <- n_biggaps * size.biggap^(n.dims) * amnt.artgaps[1] / amnt.artgaps[2]
        } else {
            n_smallgaps <- n.datapts * amnt.artgaps[2]
        }
        if (length(seed) > 0)
           set.seed((seed)%%(2^31) + 1)        
        index_values_dummy     <- which(!is.na(series.work))
        index_smallgaps        <- sort(index_values_dummy[floor(runif(n_smallgaps, min = 1, max = length(index_values_dummy) ))])
        series.work[index_smallgaps] <- NA
    }

    ## Combine both types of gaps
    index_artgaps  <- sort(c(index_smallgaps, index_biggap))       # index of all atrifical gaps
    index_allgaps  <- sort(c(index_artgaps, index_gaps))           # index of artificial and "natural" gaps
    return(list(index_biggap = index_biggap, index_artgaps = index_artgaps, index_allgaps = index_allgaps,
           index_smallgaps = index_smallgaps, series.work = series.work, index_gaps = index_gaps))
}

#################################        plot cross validation performance   ###########################################
.gapfillSSAPlot <- function(open.plot, results, amnt.iters, iter.chosen, i_best, MeasPerf) {
    if (open.plot) {
        layout(matrix(1:5, ncol = 5), widths = c(1, 1, 1, 0.1, 0.1))
        par(mar = c(0, 0, 0, 0.2), oma = c(3, 3, 2, 0.2), tcl = 0.2, mgp = c(0, 0, 100), las = 1)
    }
    zlimits <- range(c(unlist(results[['perf.all.gaps']]),
                       unlist(results[['perf.small.gaps']]),
                       unlist(results[['perf.big.gaps']])), na.rm = TRUE)
    for (i in 1:3) {
        data <- results[[5 + i]]
        image(1:amnt.iters[1], 1:amnt.iters[2], data, col = gray(100:0 / 100),
              main = c('all', 'small', 'big')[i],
              zlim = zlimits, yaxt = 'n', xlab = '', ylab = '')
        box()
        if (i == 1) {
            axis(2)
            mtext('inner iteration', side = 2, line = 1.5, las = 3)
        } else if (i == 2) {
            mtext('outer iteration', side = 1, line = 1.5)
        }
        points(iter.chosen[1], i_best[iter.chosen[1]], col = 'red', pch = 16)
    }
    legend(x = 'topright', legend = 'iter. chosen', pch = 16, col = 'red', bty = 'n')

    plot.new()
    values = seq(zlimits[1], zlimits[2], length.out = 10)
    image(1, values, matrix(seq(zlimits[1], zlimits[2], length.out = 10), ncol = 10),
          col = gray(100:1 / 100), xaxt = 'n')
    mtext(MeasPerf, side = 3, line = 0.5, cex = 0.8)
    box()
    mtext(at = c(1, 3, 5) / 6.5, c('all gaps', 'small gaps', 'big gaps'), side = 3, outer = TRUE, line = 0.5)
}



.calcSSAAllMethods <- function(
  ##title<< iterate through all SSA methods
  series.in, M, n.comp, kind, GroupEigTrpls = 'clusterify', iterindex = 0,
  SSA.methods = c('nutrlan', 'propack', 'eigen', 'svd'), run.grouping = TRUE,
  ssa.groups.t = list(), seed = c(), debugging = FALSE)
  ##description<< Helper function around ssa and reconstruct that iterates through all available
  ##              ssa methods in case the selected ones do not converge.
  ##seealso<<
  ##\code{\link{ssa}}, \code{\link{gapfillSSA}}, \code{\link{filterTseriesSSA}}

{
  ## preparation
  if (debugging)
    call.args <- as.list(environment())
  args.function <- get(ls())
  group.triples  <- 'default'
  error = FALSE
  SSA.converged = FALSE
  for (g in 1:length(SSA.methods)) {                                                  ## iterate SSA methods
    if (length(seed) > 0)                                                             ## if reproducible calculations
      set.seed(seed + g)%%(2^32)
    old.opt  = options()
    options(warn = 2)
    
    ## run SSA 
    if (kind == '1d-ssa' && (M < 50 | SSA.methods[g] == 'eigen')) {
      ssa.res <- try(ssa(x = series.in, L = M, svd.method = SSA.methods[g], kind = kind) , silent = TRUE)
    } else {
      ssa.res <- try(ssa(x = series.in, L = M, neig = n.comp, svd.method = SSA.methods[g], kind = kind) , silent = TRUE)
    } 
    
    ## try grouping
    if (!(class(ssa.res)[1] == 'try-error')) {                                        ## if ssa converged
      if (run.grouping) {                                                             ## if group eigentriples
        if (length(which(ssa.res$sigma > 0)) > 2) {                                  ## if enough eigentriples
          ssa.groups.t   <- try({
            do.call(GroupEigTrpls, list(x = ssa.res, group = which(ssa.res$sigma[1:n.comp] > 0)))
          }, silent = TRUE)
          if (class(ssa.groups.t)[1] == 'try-error')                                  ## if grouping failed
            group.triples <- 'single' 
        } else if (!(length(which(ssa.res$sigma > 0)) > 2)) {                        ## if not enough eigentriples
          group.triples  <- 'single'
        }  
        ssa.groups.local <- ssa.groups.t
      } else if (!(run.grouping)) {                                                   ## if do not group eigentriples
        ssa.groups.local <- try({sapply(ssa.groups.t, function(x)x[!is.element(x, which(ssa.res$sigma == 0)) & x <= length(ssa.res$sigma)], simplify = F)}, silent = TRUE)
      }
      options(old.opt)
      if (group.triples == 'single') {                                                ## if grouping failed
        if (kind == '1d-ssa') {
          print('Grouping of eigentriples failed. Using single eigentriples for reconstruction.')
          ##TODO remove this stuff
          if (debugging) {
            session.stuff <- sessionInfo()
            save(session.stuff, args.function, g, seed, call.args,
                file = paste('grouping_error_', as.numeric(Sys.time()),'.RData', sep = ''))
          }          
        }
        ssa.groups.t     <- as.list(1:n.comp)[which(ssa.res$sigma != 0)]
        ssa.groups.local <- ssa.groups.t
        error            <- TRUE
      }
      SSA.converged = TRUE
      break
      
      ## handle non convergence 
    } else if (class(ssa.res)[1] == 'try-error') {                                      ## if ssa did not converge
      options(old.opt)
      ##TODO remove this debugging info
      if (debugging) {
        str.time <- gsub('[[:space:]]', '-', gsub('[[:punct:]]', '-', as.character(Sys.time())))
        path.debug <- file.path('/Net', 'Groups', 'BGI', 'tmp', 'jbuttlar', 
                                'Cluster_jobs_debugging', sub('/Net/Groups/BGI/', '', getwd()))
        file.name.debug  <- paste(path.debug, '/debug_convergence_', str.time, sep = '')            
        dump.frames(dumpto = file.name.debug, to.file = TRUE)
        printStatus(paste('Saved debugging workspace to file ', file.name.debug, '.rda', sep = ''))
      }
    }
  }
  
  ##reconstruction  
  if (!SSA.converged) {
    printStatus(paste('SSA did not converge with all methods at iter ', iterindex, '. Returning input series.', sep = ''))
    recstr.res <- series.in
    error      <- TRUE
  } else {
    force.continue <- switch(SSA.methods[g], eigen = FALSE, svd = FALSE, nutrlan = TRUE, propack = TRUE)
    recstr.res <- lapply(reconstruct(ssa.res,  groups = ssa.groups.local, force.continue = force.continue), as.vector)
  }
  ##value<< ssa results.
  return(list(recstr.res = recstr.res, ssa.res = ssa.res, ssa.groups.t = ssa.groups.t,
          method.used = SSA.methods[g], error = error))
}

.getBestIteration <- function(
##title << default way to determine best SSA fit. 
    perf.matrix ##<< performance matrix (should be one of recstr_perf_a, recstr_perf_b or
                ##   recstr_perf_s).
)
  ##description<<
  ## This is the standard function that determines the best fit for the SSA gapfilling
  ## routine (gapfillSSA) and is supplied to its function call per default.
  ##details<<
  ## This is the function used per default in gapfillSSA() for cross validation.
  ## It determines the best fit between values and reconstruction at "artificial gap"
  ## positions amongst all outer and inner iterations.
  ##seealso<<
  ##\code{\link{gapfillSSA}}

{
  .fun.frst.nnan <- function(x) {
    if (sum(!is.na(x)) == 0) {
      NA
    } else {
      max(which(!is.na(x)))
    }
  }
  .fun.getmin <- function(x) {
    if (sum(!is.na(x)) == 0) {
      NA
    } else {
      min(x, na.rm = TRUE)
    }
  }  
  i_best     <- apply(perf.matrix, 1, .fun.frst.nnan)
  h_rows_min <- apply(perf.matrix, 1, .fun.getmin)
  if (length(h_rows_min)[1] > 1) {
    h_best     <- min(which(  h_rows_min <= min(h_rows_min, na.rm = TRUE) +
                            0.1 * (max(h_rows_min, na.rm = TRUE) - min(h_rows_min, na.rm = TRUE))   ))
  } else {
    h_best    <- 1
  }
  ##value<< list with best outer (h_best) and inner (i_best) iteration.
  best.iter <- list(h_best = h_best, i_best = i_best)
  return(best.iter)
}

