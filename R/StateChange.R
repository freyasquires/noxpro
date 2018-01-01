#' Row numbers of change
#' 
#' Returns the row number of a change in value.
#'  
#' @name StateChange
#' @param col Column of values
#' 
#' 
#' @return Row numbers where change happens for col.
#' 
#' @export 

StateChange = function(col){
  delta = diff(col)
  deltarow = which(delta!=0)
  return(deltarow)
}

   


