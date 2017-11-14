#' Same function as 'catf()' in BBmisc except for returning [str]
#' 
#' @param ...     See sprintf
#' @param file    See cat. Default is "" 
#' @param append  See cat. Default is FALSE.
#' @param newline Append newline at the end? Default is TRUE.
#'  
#' @export

catf <- function (..., file = "", append = FALSE, newline = TRUE)
{
  msg <- sprintf(...)
  cat(msg, ifelse(newline, "\n", ""),
      sep = "", file = file, append = append)
  invisible(msg)
}


#' Remove the First or Last Part of an Object
#' 
#' @param X     a data.frame or a matrix 
#' @param n     an integer. number of row to be removed from last. Same as \code{ head(X, NROW(X) - n)}. 
#'  
#' @examples
#' chop(iris, 140)
#' 
#' @export

chop <- function(X, n = 1) {
  head(X, NROW(X) - n)
}

