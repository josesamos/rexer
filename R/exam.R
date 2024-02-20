exam_env <- new.env()

#' `exam` S3 class
#'
#' Creates a `exam` object.
#'
#' @param rmd A string, rmd file, exam template.
#' @param questions A data frame, exam questions.
#' @param students A vector, instance names to generate.
#' @param instances_num An integer, number of instances to generate, if the names
#' of students are not indicated.
#' @param random A boolean, random or sequential generation.
#' @param reorder_questions A boolean, reorder questions in exam.
#' @param delivery A boolean, version to correct or delivery.
#' @param out_dir A string, output folder.
#' @param seed An integer, seed to generate random numbers.
#'
#' @return A `exam` object.
#'
#' @family exam definition
#'
#' @export
exam <-
  function(rmd = NULL,
           questions = NULL,
           students = NULL,
           instances_num = 1,
           random = TRUE,
           reorder_questions = TRUE,
           delivery = TRUE,
           out_dir = 'exam',
           seed = 173) {
    set.seed(seed)

    if (!is.null(students)) {
      instances_num <- length(students)
    } else {
      students <- num_vector(end = instances_num)
    }
    if (!is.null(out_dir)) {
      out_dir <- name_with_nexus(out_dir)
    }

    instances <- num_vector(end = instances_num)

    exam_env$exam_number <- 1
    exam_env$exam_number_str <- instances[1]
    exam_env$student_name <- students[1]
    exam_env$questions <- questions
    exam_env$next_question <- 1

    structure(list(
      rmd = rmd,
      students = students,
      instances = instances,
      random = random,
      reorder_questions = reorder_questions,
      delivery = delivery,
      out_dir = out_dir
    ),
    class = "exam")
  }


#' Generate pdf exam
#'
#' @param ex A `exam` object.
#' @param encoding A string.
#'
#' @return A `exam`.
#'
#' @family question definition
#'
#' @export
generate_pdf <- function(ex, encoding) UseMethod("generate_pdf")


#' @rdname generate_pdf
#' @export
generate_pdf.exam <- function(ex, encoding = "UTF-8") {
  for (student in ex$students) {
    rmarkdown::render(
      ex$rmd,
      "pdf_document",
      output_file = paste0(ex$out_dir, snakecase::to_snake_case(student)),
      encoding = encoding,
      params = list(a= ex$students, b = "otra prueba")
    )
    exam_env$exam_number <- exam_env$exam_number + 1
  }
  ex
}


#' Get exam number
#'
#' @return An integer.
#'
#' @examples
#'
#' n <- get_exam_number()
#'
#' @export
get_exam_number <- function() {
  exam_env$exam_number
}

#' Get exam number str
#'
#' @return A string.
#'
#' @examples
#'
#' n <- get_exam_number()
#'
#' @export
get_exam_number_str <- function() {
  exam_env$exam_number_str[exam_env$exam_number]
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
  exam_env$student_name[exam_env$exam_number]
}


#' Get next question
#'
#' @return A data frame
#'
#' @examples
#'
#' q <- get_next_question()
#'
#' @export
get_next_question <- function() {
  question <- exam_env$questions[exam_env$next_question, ]
  exam_env$next_question <- exam_env$next_question + 1
  question
}


#' Get question set
#'
#' @param n An integer, number of questions to include, with NULL all are included.
#'
#' @return A data frame
#'
#' @examples
#'
#' q <- get_question_set()
#'
#' @export
get_question_set <- function(n = NULL) {
  questions <- exam_env$questions
}
