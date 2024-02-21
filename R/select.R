
#' randomly selects the number of elements. If null, returns them all randomly reordered.
#'
#' @param vector A vector of strings.
#' @param n An integer, number of elements to select.
#'
#' @return A vector of strings.
#'
#' @export
select_random <- function(vector, n = NULL) {
  l <- length(vector)
  if (is.null(n)) {
    n <- l
  } else if (n > l | n <= 0) {
    n <- l
  }
  i <- sample.int(l, n)
  vector[i]
}


#' sequentially select an element, the one that corresponds according to the number
#' passed as a parameter.
#'
#' @param vector A vector of strings.
#' @param n An integer, number in sequence.
#'
#' @return A string.
#'
#' @export
select_sequential <- function(vector, n = 0) {
  l <- length(vector)
  i <- (n %% l) + 1
  vector[i]
}

