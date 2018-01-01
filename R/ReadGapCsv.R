#' Read csv file with possible empty lines and gaps.
#'
#' @name ReadGapCsv.R
#' @param csvfile String of path/filename for csv file to read
#' @param header Boolean. TRUE (default) if file contains a row of
#'     		column headers that shall be used as dataframe names
#' @param sep String of column seperaters, "," is default
#' @param comment_char Ignore lines starting with this character.
#'     		Default is "#".
#'
#' @return dataframe containing data in csvfile


ReadGapCsv <- function(csvfile,header=TRUE,sep=",",comment.char="#") {

  if (!file.exists(csvfile)) {
      cat("File does not exist:\n", "\t", csvfile, sep="")
      return()
  }

  # Read the input file ignoring any blank lines
  # First row is taken as the headers
  return(read.table(csvfile,
                    header = header,
                    sep = sep,
                    comment.char = comment.char,
                    strip.white = TRUE,
                    blank.lines.skip = TRUE))

}
