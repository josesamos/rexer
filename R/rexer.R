rexer_env <- new.env()
rexer_env$exam_numer <- '00'
rexer_env$student_name <- 'Prueba'

#' Get exam number
#'
#' @return A string.
#'
#' @examples
#'
#' n <- get_exam_number()
#'
#' @export
get_exam_number <- function() {
  rexer_env$exam_numer
}

#' Get student name
#'
#' @return A string.
#'
#' @examples
#'
#' n <- get_student_name()
#'
#' @export
get_student_name <- function() {
  rexer_env$student_name
}
