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

  delta <- diff(col)
  # Obtain rows that are not zero
  # Note that this includes nan's but they are ignored (?)
  deltarow <- which(delta!=0)
  
  return(deltarow)
}

   


