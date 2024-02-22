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
           examined = NULL,
           instances_num = 1,
           random = TRUE,
           reorder_questions = TRUE,
           delivery = TRUE,
           out_dir = 'exam',
           seed = 173) {
    set.seed(seed)

    if (!is.null(examined)) {
      instances_num <- length(examined)
    } else {
      examined <- num_vector(end = instances_num)
    }
    if (!is.null(out_dir)) {
      out_dir <- name_with_nexus(out_dir)
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
        delivery = delivery,
        out_dir = out_dir
      ),
      class = "exam"
    )
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
generate_pdf <- function(ex, encoding)
  UseMethod("generate_pdf")


#' @rdname generate_pdf
#' @export
generate_pdf.exam <- function(ex, encoding = "UTF-8") {
  exam_number <- 1
  for (examined in ex$examined) {
    questions <-
      interpret_questions(ex$questions,
                          exam_number,
                          ex$random,
                          ex$reorder_questions,
                          ex$delivery)
    all_questions <-
      interpret_all_questions(ex$questions,
                              exam_number,
                              ex$random,
                              ex$reorder_questions,
                              ex$delivery)

    rmarkdown::render(
      ex$rmd,
      "pdf_document",
      output_file = paste0(ex$out_dir, snakecase::to_snake_case(examined)),
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
  function(questions, exam_number, random, reorder, delivery) {
    if (reorder) {
      nq <- nrow(questions)
      r <- sample(1:nq, nq, replace = FALSE)
      questions <- questions[r, ]
    }
    txt <- ''
    for (i in 1:nrow(questions)) {
      question <-
        interpret_a_question(questions[i,], exam_number, random, delivery)
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
  function(questions, exam_number, random, reorder, delivery) {
    nq <- nrow(questions)
    if (reorder) {
      r <- sample(1:nq, nq, replace = FALSE)
    } else {
      r <- 1:nq
    }
    vq <- NULL
    for (i in r) {
      question <- interpret_a_question(questions[i, ], exam_number, random, delivery)
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
interpret_a_question <- function(question, exam_number, random, delivery) {
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
    avoid <-
      as.integer(stringr::str_extract(c(question[, "image_alt"], question[, "image"]), "[[(\\d+)]]"))
    avoid <- avoid[!is.na(avoid)]
  }

  last_sel <- 0
  i <- 0
  for (s in seq_along(others)) {
    vector <- string_to_vector(others[[s]])
    if (random) {
      sel <- select_random(vector, n = 1)
    } else {
      sel <- select_sequential(vector, n = exam_number)
    }
    last_sel <- which(vector == sel, vector)
    reorder <- select_random(vector)
    i <- i + 1
    pattern <- paste0('{{', i, '}}')
    if (grepl(pattern, txt, fixed = TRUE)) {
      reorder <- reduce_vector(reorder, sep = '')
      txt <- gsub(pattern, reorder, txt, fixed = TRUE)
    } else {
      pattern <- paste0('[[', i, ']]')
      if (!delivery) {
        if (!(i %in% avoid)) {
          sel <- paste0('**[[', i, ': ', sel, ']]**')
        }
      }
      txt <- gsub(pattern, sel, txt, fixed = TRUE)
    }
  }
  if (!delivery) {
    answer <- question[, "answer"]
    answer <- string_to_vector(answer)
    if (length(answer) > 1) {
      answer <- answer[last_sel]
    }
    if (answer != '') {
      txt <- paste0(txt,
                    '

**Answer: **', answer)
    }
  }
  txt
}
