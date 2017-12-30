#' Create calibration flags
#' 
#' Finds when calibration is happening by looking at valve status. Asks user if good or bad calibration
#' and creates a calibration flag (1 for good and 0 for bad). 
#' @name CreateCalFlags
#' @param df Dataframe of raw AQD NOx output 
#' @param calcol Column name for calibration valve staus (default is "NO_cal_valve")
#' 
#' @return Dataframe df with additional column of calibration flag. Column name is "cal_flag"
#' 
#' @export 

CreateCalFlags = function(df, calcol = "NO_cal_valve"){
  
  #Find column number of calcol
  calcolnum <-  match(calcol, names(df))
  if (is.na(calcolnum)){
    print(paste(calcol," not found"))
    return()  
  }
  
  #Create a UNIX.TS column (required for find_ranges function)
  if (!("UNIX.TS" %in% names(df))){
    df <- ExcelToUnix(df)
  }

  #Create Table of ranges where calibration valve is active
  calranges = find_ranges(df, calcolnum)

  #ask user to state whether the cal should be used or not
  calranges = plyr::adply(calranges, 1,
                          function(x) GoodCalPrompt(x),
                          .progress = "none")

  #Initialise a new flag column 
  df$cal_flag = 0 
  df$cal_on = 0
  
  #Flag the calibrations that the user has defined as good with 1
  for (i  in 1:nrow(calranges)){
    range_on = (calranges$startrow[i]-1):(calranges$endrow[i]+1)
    range_flag = calranges$startrow[i]:calranges$endrow[i]
    df$cal_on[range_on] = 1
    if(calranges$good[i] == 1){
      df$cal_flag[range_flag] = 1
    }
  }
  return(df)
}  



#' Good Calibration Prompt
#' 
#' Prompt used to assertain where cals are good or bad, requires user input.
#' Based on func_goodcalprompt.R from willdrysdale/wsdmiscr. 
#' 
#' @name GoodCalPrompt
#' @param d Data frame with start and end times 
#' 
#' @return Dataframe d with additional 'good' column
#' 
#' @export   

GoodCalPrompt = function(d){

  valid <- 0
  while (valid == 0) {
    # If invalid response the ask again
    i = readline(prompt = paste("Was the cal between, ",as.character(d$start)," and ",as.character(d$end)," good? [Y/n] : ",sep = ""))

    if (i == ''){
      i <- 'y'
    }
    if (tolower(i) %in% c("y","yes")) {
      d$good <- 1
      valid <- 1
    }
    else if (tolower(i) %in% c("n","no")) {
      d$good <- 0
      valid <- 1
    }
    else {
      #valid <- 0
      print("Invalid response")
    }
  }
  return(d)
}