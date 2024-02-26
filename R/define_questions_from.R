#' Define a question
#'
#' Defines a question with random components.
#'
#' If an image is included in the question, text in the `image_alt` field associated
#' with it must also be included.
#'
#' Following the answer, options can be defined to fill in the gaps that have been
#' specified in the question.
#'
#' Both the answer and the options are formed by a vector of strings, from which
#' one is chosen to formulate the question and select the answer. To represent a
#' vector of strings in a cell, the function `vector_to_string()` is used, which
#' includes a separator ("<|>") between the vector elements to generate a string.
#'
#' @param ex An `exam` object.
#' @param type A character, 'p' indicates whether the question starts on a new page.
#' @param question A string, statement of the question.
#' @param image A string, optional, image file to include in the question.
#' @param image_alt A string, description of the image to include in the question.
#' @param answer A string, correct answer to the question.
#' @param ... A string, options for the gaps in the question.
#'
#' @return An `exam`.
#'
#' @family question definition
#' @seealso \code{\link{exam}}, \code{\link{vector_to_string}}
#'
#' @examples
#'
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 10,
#'   random = TRUE,
#'   reorder_questions = TRUE,
#'   select_n_questions = NULL
#' ) |>
#'   define_a_question(
#'     type = 'p',
#'     question = 'What is the three-letter country code (ISO 3166-1 alpha-3) for
#'     the country represented in the figure below?',
#'     image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
#'     image_alt = 'Country outline.',
#'     answer = 'ESP<|>CHL<|>NZL<|>ITA',
#'     "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
#'   )
#'
#' @export
define_a_question <- function(ex,
                              type,
                              question,
                              image,
                              image_alt,
                              answer,
                              ...)
  UseMethod("define_a_question")


#' @rdname define_a_question
#' @export
define_a_question.exam <- function(ex,
                                   type = '',
                                   question = '',
                                   image = '',
                                   image_alt = '',
                                   answer = '',
                                   ...) {
  if (image != '') {
    stopifnot('If an image is included, the associated alt field must also be defined.' = image_alt != '')
  }

  others <- list(...)
  wrong <- NULL
  for (s in seq_along(others)) {
    w <- trimws(others[[s]])
    if (length(w) > 1) {
      w <- vector_to_string(w)
    }
    if (nchar(w) > 0) {
      wrong <- c(wrong, w)
    }
  }
  if (length(answer) > 1) {
    answer <- vector_to_string(answer)
  }
  n <- length(wrong)
  nq <- data.frame(
    type = type,
    question = question,
    image = image,
    image_alt = image_alt,
    answer = answer
  )
  if (n > 0) {
    for (i in 1:n) {
      nq[1, paste0('a_', i)] <- wrong[i]
    }
  }
  if (n < ex$a_n) {
    for (i in (n + 1):ex$a_n) {
      nq[1, paste0('a_', i)] <- ''
    }
  }
  if (nrow(ex$questions) > 0) {
    if (n > ex$a_n) {
      for (i in (ex$a_n + 1):n) {
        ex$questions[, paste0('a_', i)] <- ''
      }
      ex$a_n <- n
    }
    ex$questions <- rbind(ex$questions, nq)
  } else {
    if (n > ex$a_n) {
      ex$a_n <- n
    }
    ex$questions <- nq
  }
  ex
}


#' Define questions from a data frame
#'
#' Each row in the text data frame is interpreted as a question. We only need to define
#' the columns that we are going to use; the rest of the columns are taken by default.
#'
#' Both the answer and the options are formed by a vector of strings, from which
#' one is chosen to formulate the question and select the answer. To represent a
#' vector of strings in a cell, the function `vector_to_string()` is used, which
#' includes a separator ("<|>") between the vector elements to generate a string.
#'
#' @param ex An `exam` object.
#' @param df A data frame containing questions.
#'
#' @return An `exam`.
#'
#' @family question definition
#' @seealso \code{\link{exam}}, \code{\link{vector_to_string}}
#'
#' @examples
#'
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' questions <- system.file("extdata/questions.csv", package = "rexer")
#' q <- read_question_csv(questions)
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 10,
#'   random = TRUE,
#'   reorder_questions = TRUE,
#'   select_n_questions = NULL
#' ) |>
#'   define_questions(q)
#'
#' @export
define_questions <- function(ex, df)
  UseMethod("define_questions")

#' @rdname define_questions
#' @export
define_questions.exam <- function(ex, df) {
  attributes <- names(df)
  df[, attributes] <-
    data.frame(lapply(df[, attributes], as.character), stringsAsFactors = FALSE)
  df[, attributes] <-
    apply(df[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ''))
  attributes <- snakecase::to_snake_case(attributes)
  names(df) <- attributes
  for (opcional in c('type', 'image', 'image_alt')) {
    if (!(opcional %in% attributes)) {
      df[, opcional] <- ''
    }
  }
  rest <-
    setdiff(attributes,
            c("type", "question", "image", "image_alt", "answer"))
  for (i in 1:nrow(df)) {
    text <- paste0(
      'define_a_question(ex, type = "',
      df[i, 'type'],
      '", question = "',
      df[i, 'question'],
      '", image = "',
      df[i, 'image'],
      '", image_alt = "',
      df[i, 'image_alt'],
      '", answer = ',
      string_to_string_vector(df[i, 'answer'][[1]])
    )
    j <- 0
    for (r in rest) {
      if (df[i, r][[1]] != '') {
        j <- j + 1
        text <- paste0(text,
                       ", a_", j, " = ", string_to_string_vector(df[i, r][[1]]))
      }
    }
    text <- paste0(text, ")")
    ex <- eval(parse(text = text))
  }
  ex
}


#' Define questions from a csv file
#'
#' Each row in the text file is interpreted as a question. We only need to define
#' the columns that we are going to use; the rest of the columns are taken by default.
#'
#' Both the answer and the options are formed by a vector of strings, from which
#' one is chosen to formulate the question and select the answer. To represent a
#' vector of strings in a cell, the function `vector_to_string()` is used, which
#' includes a separator ("<|>") between the vector elements to generate a string.
#'
#' @param ex An `exam` object.
#' @param file A string, name of a text file.
#' @param sep Column separator character ("," or ";").
#'
#' @return An `exam`.
#'
#' @family question definition
#' @seealso \code{\link{exam}}, \code{\link{vector_to_string}}
#'
#' @examples
#'
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' questions <- system.file("extdata/questions.csv", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 10,
#'   random = TRUE,
#'   reorder_questions = TRUE,
#'   select_n_questions = NULL
#' ) |>
#'   define_questions_from_csv(questions)
#'
#' @export
define_questions_from_csv <- function(ex, file, sep)
  UseMethod("define_questions_from_csv")

#' @rdname define_questions_from_csv
#' @export
define_questions_from_csv.exam <- function(ex, file, sep = ',') {
  df <- readr::read_delim(
    file,
    delim = sep,
    col_types = readr::cols(.default = readr::col_character())
  )
  define_questions(ex, df)
}


#' Define questions from a Excel file
#'
#' Each row in the Excel file is interpreted as a question. We only need to define
#' the columns that we are going to use; the rest of the columns are taken by default.
#'
#' In addition to the file, we can indicate the sheet by its name or index. If we
#' do not indicate anything, it considers the first sheet.
#'
#' Both the answer and the options are formed by a vector of strings, from which
#' one is chosen to formulate the question and select the answer. To represent a
#' vector of strings in a cell, the function `vector_to_string()` is used, which
#' includes a separator ("<|>") between the vector elements to generate a string.
#'
#' @param ex An `exam` object.
#' @param file A string, name of an Excel file.
#' @param sheet_index A number, sheet index in the workbook.
#' @param sheet_name A string, sheet name.
#'
#' @return An `exam`.
#'
#' @family question definition
#' @seealso \code{\link{exam}}, \code{\link{vector_to_string}}
#'
#' @examples
#'
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' questions <- system.file("extdata/questions.xlsx", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 10,
#'   random = TRUE,
#'   reorder_questions = TRUE,
#'   select_n_questions = NULL
#' ) |>
#'   define_questions_from_excel(questions)
#'
#' @export
define_questions_from_excel <- function(ex,
                                        file,
                                        sheet_index,
                                        sheet_name)
  UseMethod("define_questions_from_excel")

#' @rdname define_questions_from_excel
#' @export
define_questions_from_excel.exam <- function(ex,
                                             file,
                                             sheet_index = NULL,
                                             sheet_name = NULL) {
  if (is.null(sheet_index) & is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)
  } else if (is.null(sheet_name)) {
    sheet_name <- readxl::excel_sheets(file)[sheet_index]
  }
  sheet_name <- sheet_name[1]
  df <- suppressMessages(
    readxl::read_excel(
      file,
      sheet = sheet_name,
      col_names = TRUE,
      col_types = "text",
      trim_ws = TRUE
    )
  )
  define_questions(ex, df)
}
