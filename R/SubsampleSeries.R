#' Sub-sample a time series based on another time series and return means
#'
#' A high-resolution sequence is subsampled so that its data points
#' match that of a lower-resolution sequence. These sequences will
#' usually be time series but don't have to be. The high-resolution
#' sequence is binned such that the bin centres are at the points
#' of the low-resolution data and all high resolution data within
#' a single bin is averaged.
#'
#' @name SubsampleSeries.R
#' @param x.hres Sequence or array of high-resolution x-axis data points
#'           This may be a series of POSIX datetimes
#'           If x.hres == 'test' then args are ignored and test run
#' @param y.hres Corresponding sequence of high-resolutuon y values
#'           x.lres: Sequence of low-resolution x values. Must be some
#'           overlap between x.hres and x.lres
#' @param func Function to apply to the aggregate command over each bin
#'           Default is mean
#' @param testplot If True then create a plot to test data. Always happens
#'           if x.hres == 'test'. Default is FALSE
#'
#' @return y.lres Rebinned y values for each x.lres point
#'
#' @export


SubsampleSeries <- function(x.hres,y.hres,x.lres,
                            func=mean,testplot=FALSE) {

  # Sort out testing
  if (x.hres == 'test') {
    x.hres <- seq(as.POSIXct('2017-04-24 00:00:00'),
                  as.POSIXct('2017-04-25 00:00:00'),
                  by='10 min')
    y.hres <- sinpi(seq(0,1,length=length(x.hres))) + 0.5

    x.lres <- seq(as.POSIXct('2017-04-24 00:00:00'),
                  as.POSIXct('2017-04-25 00:00:00'),
                  by='60 min')

    testplot <- TRUE
  }

  # Find indicies in low res data that overlap high res data
  x.lres.index <- x.lres >= min(x.hres) & x.lres <= max(x.hres)

  x.lres.sel <- x.lres[x.lres.index]

  # Create x bins for averaging high-resolution data in
  # Note that end bins will be half width.
  bin.lower <- x.lres.sel[2:length(x.lres.sel)-1] + diff(x.lres.sel)/2
  bin.lower <- append(bin.lower, x.lres.sel[1],0)
  bin.thresholds <- append(bin.lower,
                           x.lres.sel[length(x.lres.sel)],
                           length(bin.lower)+1)

  # Find the bins in which each of the high-resolution x-vals belong
  bin.indices <- findInterval(x.hres,bin.thresholds,all.inside=TRUE)

  # Apply the desired function to the y.hres within each bin
  y.lres <- aggregate(y.hres, list(bin.indices), func)$x

  if (testplot == TRUE) {
    plot(x.hres,y.hres,type='l',col='blue',yaxs='i',ylim=c(0, 2.5))
    abline(v=x.lres,col='green')
    lines(x.lres,y.lres,type='p',col='green',pch=10)
  }

  return(y.lres)
}

