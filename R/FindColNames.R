#' Return column names of dataframe with matched strings
#'
#' Function to find column names in a dataframe that contain a given
#' substring. If no column headings are given then return all columns
#' in dataframe.
#'
#' @name FindColNames.R
#' @param df Dataframe to be interrogated
#' @param col_match Vector of strings or substrings (single 
#'     		character string is also ok) to be searched for in dataframe
#'     		column headings. Default is "all" in which case all column
#'     		headings are returned.
#' @param preserve_case Boolean. If TRUE (default) then match case, if
#'     		FALSE then ignore string case when matching.
#'
#' @return Vector of column headings that match the col_match
#'     		 substrings. If no matches are found then returns empty vector


FindColNames <- function(df,col.match="all",preserve.case=TRUE) {

  # Obtains all headings in dataframe
  # TODO: Make possible to pass names(df) to func as well as df
  headings <- names(df)

  if (class(col.match) == "character") {
    # If col_str given as a single string then convert to list
    col.match <- c(col.match)
  }

  if (col.match[1] == "all") {
    return(headings)
  }

  # Find all heading strings that match any of the col.match strings.s
  matches <- sapply(col.match,grepl,headings,ignore.case=!preserve.case)
  matches.any <- apply(matches,1,any)
  
  return(headings[matches.any])
}
