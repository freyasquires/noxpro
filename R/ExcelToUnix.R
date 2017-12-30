#' Create unix timestamp from excel time
#' 
#' Some functions require unix timestamp. This creates a unix timestamp column from excel time in the raw
#' AQD NOx output file.
#' @name ExcelToUnix
#' @param df Data frame of raw AQD NOx output 
#' 
#' @return Dataframe df with column containing a unix timestamp. Column name is "UNIX.TS"
#' 
#' @export 

ExcelToUnix = function(df){
  unix_tmp = waclr::parse_excel_date(df$TheTime)
  df$UNIX.TS = as.numeric(unix_tmp)
  return(df)
  }