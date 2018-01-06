#' Interpolate a low-resolution data series onto high-resolution series
#'
#' 
#' 
#' 
#' 
#' 

#' @name InterpSeries
#' 
#' 
#' 
#' 


InterpSeries = function(df,series_col='date',cols=c()) {







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
  if (class(col) == "character" & !(col %in% names(df))) {
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