
#' generates a vector of numbers in string format, for the  interval.
#'
#' @param start An integer, start of interval.
#' @param end An integer, end of interval.
#'
#' @return A vector of strings.
#' @keywords internal
num_vector <- function(start = 1, end) {
  values <- start:end
  lmax <- max(nchar(end))
  res <- sprintf(paste0('%0', lmax, 'd'), values)
  res
}



#' Name with nexus
#'
#' Given a name, if it ends in "/" the nexus is the empty string, otherwise it
#' is "/". Add the nexus.
#'
#' @param name A string.
#'
#' @return A string.
#' @keywords internal
name_with_nexus <- function(name) {
  l <- nchar(name)
  c <- substr(name, start = l, stop = l)
  res <- name
  for (i in seq_along(c)) {
    if (c[i] != "/") {
      res[i] <- paste0(name[i], "/")
    }
  }
  res
}

