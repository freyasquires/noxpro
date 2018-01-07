#' Interpolate a low-resolution data series onto high-resolution series
#'
#' A low-resolution data series in dataframe df is linearly interpolated onto
#' a high-resolution series. This hi-res series may be time or any other
#' monotonic series. Linear interpolation is used and extrapolation at
#' either end is given as a constant value, ie approxfun with rule=2 is used.
#' 
#' @name InterpSeries
#' @param df Dataframe containing both series_col and cols columns
#' @param cols Column name or number or vector thereof for column/s to
#'        interpolate
#' @param series_col Column name or number of series of points to interpolate 
#'        cols on to
#' 
#' @return df with additional columns with interpolated data. If cols is given
#'         as names then additional columns are named col_interp where col is
#'         in cols. If cols is a column number then new columns are added to the
#'         end of df.
#'
#' @export

InterpSeries = function(df,cols,series_col='date') {


  # Make cols into a vector for looping if not already
  if (!is.vector(cols)) {
    cols = c(cols)
  }

  # If series_col is not in df then return df unmodified
  if (!(series_col %in% names(df))) {
    print(paste(series_col," not found"))
    return(df)
  }

  # Do not use rows where series_col is NAN
  series.good <- df[,series_col][!is.na(df[,series_col])]
  
  for (col in cols) {

    # Create new col name or number
    if (is.numeric(col)) {
      if (col > ncol(df)) {
        # column number given not in df
        next
      }
      col.new <- ncol(df) + 1
    }
    if (is.character(col)) {
      if (!(series_col %in% names(df))) {
        # col name not in df
        next
      }
      col.new <- paste(col,'_interp',sep='')
    }

    # Only use data rows without NANs
    col.good <- df[,col][!is.na(df[,col])]

    # Create interpolation function
    interp_func <- approxfun(series.good[!is.na(df[,col])],col.good,rule=2)

    # Create new col and populate with interpolated data
    df[col.new] <- interp_func(series.good)
  }

  return(df)

}


#' Take raw aircraft cal/zero values and strip out replicas for interpolation
#' 
#' Dataframe columns for calibration and zeroing are a constant value until
#' a new value is calculated. If an interpolated value is required then this
#' function replaces most of the values with NAs, leaving only the first
#' value after a change. This tidied column data can then be fed to InterpSeries
#' 
#' @name TidyCalVals
#' @param df Dataframe with appropriate columns
#' @param col Column name or number which will be tidied. If invalid for df then
#'        df is returned unaltered.
#' 
#' @return Dataframe df with column col tidied
#'
#' @export

TidyCalVals = function(df,col) {

  # Test whether col is valid for dataframe df
  if (is.character(col) & !(col %in% names(df))) {
    # col variable not a named column of df
    return(df)
  }
  if (is.numeric(col) & !(col %in% range(0:ncol(df)))) {
    # If column number is given then check that it is valid
    return(df)
  }

  # Find where values change in col
  change_rows <- StateChange(df[,col])

  if (length(change_rows)==0) {
    # No changes in value
    # NOT SURE WHAT TO DO IN THIS SITUATION
    return(df)
  }

  # Save existing values
  change_vals <- df[,col][change_rows]

  # Make entire col column NA then reinstate saved values
  df[,col] <- NA
  df[,col][change_rows] <- change_vals

  return(df)
}