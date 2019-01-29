#' remove different kinds of missing values from a vector
#'
#' namely NULL, NA, "" and any non-finite values
hasdata<-function (x, return.index = F) {
  index <- which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))
  value <- x[which(!is.null(x) & !is.na(x) & x != "" & !is.infinite(x))]
  if (return.index) {
    return(index)
  }
  return(value)
}
