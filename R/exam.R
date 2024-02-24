#' `exam` S3 class
#'
#' Creates a `exam` object.
#'
#' @param rmd A string, rmd file, exam template.
#' @param examined A vector, instance names to generate.
#' @param instances_num An integer, number of instances to generate, if the names
#' of examined are not indicated.
#' @param random A boolean, random or sequential generation.
#' @param reorder_questions A boolean, reorder questions in exam.
#' @param select_n_questions An integer, number of questions to include.
#'
#' @return A `exam` object.
#'
#' @family exam definition
#'
#' @export
exam <-
  function(rmd = NULL,
           examined = NULL,
           instances_num = 1,
           random = TRUE,
           reorder_questions = TRUE,
           select_n_questions = NULL) {

    if (!is.null(examined)) {
      instances_num <- length(examined)
    } else {
      examined <- num_vector(end = instances_num)
    }

    instances <- num_vector(end = instances_num)

    questions <-  data.frame(
      type = character(),
      question = character(),
      image = character(),
      image_alt = character(),
      answer = character(),
      a_1 = character(),
      a_2 = character(),
      a_3 = character(),
      stringsAsFactors = FALSE
    )

    structure(
      list(
        rmd = rmd,
        a_n = 3,
        questions = questions,
        examined = examined,
        instances = instances,
        random = random,
        reorder_questions = reorder_questions,
        select_n_questions = select_n_questions,
        delivery = TRUE,
        seed = 173
      ),
      class = "exam"
    )
  }


#' generate the exam document
#'
#' @param ex A `exam` object.
#' @param out_dir A string, output folder.
#' @param output_format A vector of strings.
#' @param encoding A string.
#'
#' @return A `exam` object.
#'
#' @family question definition
#'
#' @export
generate_document <- function(ex, out_dir, output_format, encoding)
  UseMethod("generate_document")

#' @rdname generate_document
#' @export
generate_document.exam <- function(ex,
                                   out_dir = NULL,
                                   output_format = "pdf_document",
                                   encoding = "UTF-8") {
  if (!is.null(out_dir)) {
    out_dir <- name_with_nexus(out_dir)
  }
  set.seed(ex$seed)
  exam_number <- 1
  n <- nrow(ex$questions)
  if (is.null(ex$select_n_questions)) {
    select_n_questions <- n
  } else if (ex$select_n_questions > n) {
    select_n_questions <- n
  } else {
    select_n_questions <- ex$select_n_questions
  }
  sel_questions <- ex$questions
  for (examined in ex$examined) {
    if (select_n_questions < n) {
      i <- sample.int(n, select_n_questions)
      if (!ex$reorder_questions) {
        i <- sort(i)
      }
      sel_questions <- ex$questions[i,]
    }
    questions <-
      interpret_questions(sel_questions,
                          exam_number,
                          ex$random,
                          ex$reorder_questions,
                          ex$delivery)
    all_questions <-
      interpret_all_questions(sel_questions,
                              exam_number,
                              ex$random,
                              ex$reorder_questions,
                              ex$delivery)

    rmarkdown::render(
      ex$rmd,
      output_format,
      output_file = paste0(out_dir, snakecase::to_snake_case(examined)),
      encoding = encoding,
      params = list(
        exam_number = exam_number,
        exam_number_str = ex$instances[exam_number],
        examined = examined,
        questions = questions,
        all_questions = all_questions
      )
    )
    exam_number <- exam_number + 1
  }
  ex
}


#' generate the exam document
#'
#' @param ex A `exam` object.
#' @param out_dir A string, output folder.
#' @param output_format A vector of strings.
#' @param encoding A string.
#'
#' @return A `exam` object.
#'
#' @family question definition
#'
#' @export
generate_correction_document <- function(ex, out_dir, output_format, encoding)
  UseMethod("generate_correction_document")


#' @rdname generate_correction_document
#' @export
generate_correction_document.exam <-
  function(ex,
           out_dir = NULL,
           output_format = "pdf_document",
           encoding = "UTF-8") {
    ex_corr <- ex
    ex_corr$delivery <- FALSE
    ex_corr <- generate_document(ex_corr, out_dir, output_format, encoding)
    ex
  }


#' interpret all question
#'
#' @param questions A data frame, questions.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param reorder A boolean, reorder questions.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string.
#' @keywords internal
interpret_all_questions <-
  function(questions,
           exam_number,
           random,
           reorder,
           delivery) {
    if (reorder) {
      nq <- nrow(questions)
      r <- sample(1:nq, nq, replace = FALSE)
      questions <- questions[r,]
    }
    txt <- ''
    for (i in 1:nrow(questions)) {
      question <-
        interpret_a_question(questions[i, ], exam_number, random, delivery)
      if (questions$type[i] == 'p' & i > 1) {
        txt <- paste0(txt, '
\\newpage
')
      }
      txt <- paste0(txt, '

**', i, '.** ', question)
    }
    txt
  }


#' interpret questions
#'
#' @param questions A data frame, questions.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param reorder A boolean, reorder questions.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string vector.
#' @keywords internal
interpret_questions <-
  function(questions,
           exam_number,
           random,
           reorder,
           delivery) {
    nq <- nrow(questions)
    if (reorder) {
      r <- sample(1:nq, nq, replace = FALSE)
    } else {
      r <- 1:nq
    }
    vq <- NULL
    for (i in r) {
      question <-
        interpret_a_question(questions[i,], exam_number, random, delivery)
      vq <- c(vq, question)
    }
    vq
  }


#' interpret a question.
#'
#' @param question A data frame, question.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string.
#' @keywords internal
interpret_a_question <-
  function(question, exam_number, random, delivery) {

    browser()

    names <- names(question)
    base <- c("type", "question", "image", "image_alt", "answer")
    values <- setdiff(names, base)
    others <- question[, values]
    others <- others[, others != '']
    txt <- question[, "question"]

    avoid <- integer(0)
    if (question[, "image"] != '') {
      txt <- paste0(txt,
                    '

![',
                    question[, "image_alt"],
                    '](',
                    question[, "image"],
                    ')
')
      r <- unlist(stringr::str_extract_all(question[, "image"], "\\[\\[\\d\\]\\]"))
      avoid <- as.integer(stringr::str_extract(r, "\\d"))
      avoid <- avoid[!is.na(avoid)]
    }
    txt <- reorder_items(txt)

    answer <- question[, "answer"]
    answer <- string_to_vector(answer)
    n_answers <- length(answer)
    if (n_answers > 1) {
      if (random) {
        sel_answer <- sample.int(n_answers, 1)
        answer <- answer[sel_answer]
      } else {
        answer <- select_sequential(answer, n = exam_number)
      }
    } else {
      sel_answer <- 0
    }
    i <- 0
    for (s in seq_along(others)) {
      vector <- string_to_vector(others[[s]])
      if (random) {
        if (sel_answer > 0) {
          sel <- select_sequential(vector, n = sel_answer)
        } else {
          sel <- select_random(vector, n = 1)
        }
      } else {
        sel <- select_sequential(vector, n = exam_number)
      }
      i <- i + 1
      pattern <- paste0('[[', i, ']]')
      if (!delivery) {
        if (!(i %in% avoid)) {
          sel <- paste0('**[[', i, ': ', sel, ']]**')
        }
      }
      txt <- gsub(pattern, sel, txt, fixed = TRUE)
    }
    if (!delivery) {
      if (length(answer) > 0) {
        txt <- paste0(txt,
                      '

---

', answer, '


---

')
      }
    }
    txt
  }


#' reorder items.
#'
#' @param txt A string.
#'
#' @return A string.
#' @keywords internal
reorder_items <- function(txt) {
  x <- stringr::str_extract_all(txt, "\\{\\{([[:print:]]+)\\}\\}")
  for (i in seq_along(x)) {
    r <- unlist(stringr::str_extract_all(x[[i]], "\\[\\[\\d\\]\\]"))
    v <- as.integer(stringr::str_extract(r, "\\d"))
    v <- v[!is.na(v)]
    l <- length(v)
    s <- sample.int(l, l)
    w <- v[s]
  }

  length(x)




}
