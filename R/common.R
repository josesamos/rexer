
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


# string to vector ----------------------------------------------------

#' Transforms string into a vector of strings.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_vector <- function(str) {
  if (length(str) == 0) {
    res <- NULL
  } else if (str == "") {
    res <- NULL
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
  }
  res
}


# string to string vector ----------------------------------------------

#' Transforms string into a vector in string format.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_string_vector <- function(str) {
  if (length(str) == 0) {
    res <- "''"
  } else if (str == "") {
    res <- "''"
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
    if (length(res) > 1) {
      res <- paste(res, collapse = "', '")
      res <- paste0("c('", res, "')")
    } else {
      res <- paste0("'", res, "'")
    }
  }
  res
}


#' reduce vector by sep.
#'
#' @param vector A vector of strings.
#' @param sep A string, separator to use.
#' @param italics A boolean.
#'
#' @return A string.
#' @keywords internal
reduce_vector <- function(vector, sep = '\n', italics = FALSE) {
  if (italics) {
    sep <- paste0('*', sep, '*')
  }
  res <- paste(vector, collapse = sep)
  if (italics) {
    res <- paste0('*', res, '*')
  }
  res
}

