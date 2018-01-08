#' Row numbers of change
#' 
#' Returns the row number of a change in value in a given column.
#'  
#' @name StateChange
#' @param col Column of values
#' 
#' @return Row numbers where change happens for col.
#' 
#' @export 

StateChange = function(col){

  # Replace any NAs with the value from the next row
  # This is primarily to get rid of single NAs between
  # state changes. The state change will then be given
  # as the value -before- the NA which is probably what
  # is wanted. Multiple NAs will be reduced in duration by 1.
  na.index <- which(is.na(col))
  col[na.index] <- col[na.index+1]
  
  delta <- diff(col)

  # Obtain rows that are not zero
  deltarow <- which(delta!=0)
  
  return(deltarow)
}

   


