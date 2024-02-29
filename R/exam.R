#' `exam` S3 class
#'
#' Creates an `exam` object.
#'
#' A test is based on an Rmd template that has predefined parameters whose values
#' are filled in using the functions of this object. In the `rmd` parameter, we
#' specify the template file.
#'
#' From the template, we generate multiple instances of the exam. We can specify
#' the instances to generate in two ways: by indicating a vector of examinee names
#' (using the `examinees` parameter) or by specifying the number of instances to
#' generate (using the `instances_num` parameter). If both are indicated, the
#' examinee names take precedence.
#'
#' We can generate the instances either randomly or sequentially, depending on the
#' instance number we generate. This is controlled by the `random` parameter.
#'
#' Additionally, in each test, we can include the exercises in the same order as
#' they are defined or in random order. This is indicated by the `reorder_exercises`
#' parameter.
#'
#' Finally, using the `select_n_exercises` parameter, we can specify the number
#' of exercises to include in each test. From all available exercises, the quantity
#' specified in this parameter will be randomly selected. By default, all defined
#' exercises are included.
#'
#' @param rmd A string representing the path to the Rmd file, the exam template.
#' @param examinees A vector of strings, representing the names of instances to
#' generate.
#' @param instances_num An integer, representing the number of instances to generate
#' if the examinee names are not provided.
#' @param random A boolean, indicating whether to generate instances randomly or
#' sequentially.
#' @param reorder_exercises A boolean, indicating whether to reorder exercises in
#' the exam.
#' @param select_n_exercises An integer, representing the number of exercises to
#' include.
#' @return An `exam` object.
#'
#' @family exam definition
#' @seealso \code{\link{define_an_exercise}}
#'
#' @examples
#'
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 10,
#'   random = TRUE,
#'   reorder_exercises = TRUE,
#'   select_n_exercises = NULL
#' )
#'
#' @export
exam <-
  function(rmd = NULL,
           examinees = NULL,
           instances_num = 1,
           random = TRUE,
           reorder_exercises = TRUE,
           select_n_exercises = NULL) {
    stopifnot("We need a template to define an exam." = !is.null(rmd))
    if (!is.null(examinees)) {
      examinees <- unique(examinees)
      instances_num <- length(examinees)
    } else {
      examinees <- num_vector(end = instances_num)
    }
    instances <- num_vector(end = instances_num)

    exercises <-  data.frame(
      type = character(),
      statement = character(),
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
        exercises = exercises,
        examinees = examinees,
        instances = instances,
        random = random,
        reorder_exercises = reorder_exercises,
        select_n_exercises = select_n_exercises,
        delivery = TRUE,
        seed = 173
      ),
      class = "exam"
    )
  }


#' Generate the exam document
#'
#' From an exam object, we generate different instances of the exam to deliver to
#' the individuals being examined. To do this, we need to specify the folder where
#' they will be generated (using parameter `out_dir`), the output format (using
#' parameter `output_format`), the encoding (using parameter `encoding`), and whether
#' we want each exercise to start on a new page, include exercises until the pages
#' are filled, or preserve the definition of the exercise in this regard (using
#' parameter `new_pages` with the values 'all', 'none', or NULL).
#'
#' @param ex An `exam` object.
#' @param out_dir A string indicating the output folder.
#' @param output_format A vector of strings specifying the desired output formats.
#' @param encoding A string specifying the encoding.
#' @param new_pages A string with the values 'all', 'none', or NULL.
#'
#' @return An `exam` object.
#'
#' @family exam definition
#'
#' @examples
#'
#' \donttest{
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' exercises <- system.file("extdata/exercises.csv", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 1,
#'   random = TRUE,
#'   reorder_exercises = TRUE
#' ) |>
#'   define_exercises_from_csv(exercises)
#' # ex |>
#' #   generate_document(out_dir = tempdir(), new_pages = 'all')
#' }
#'
#' @export
generate_document <-
  function(ex,
           out_dir,
           output_format,
           encoding,
           new_pages)
    UseMethod("generate_document")

#' @rdname generate_document
#' @export
generate_document.exam <- function(ex,
                                   out_dir = NULL,
                                   output_format = "pdf_document",
                                   encoding = "UTF-8",
                                   new_pages = NULL) {
  if (!is.null(out_dir)) {
    out_dir <- name_with_nexus(out_dir)
  }
  set.seed(ex$seed)
  exam_number <- 1
  n <- nrow(ex$exercises)
  if (is.null(ex$select_n_exercises)) {
    select_n_exercises <- n
  } else if (ex$select_n_exercises > n) {
    select_n_exercises <- n
  } else {
    select_n_exercises <- ex$select_n_exercises
  }
  sel_exercises <- ex$exercises
  if (!is.null(new_pages)) {
    if (tolower(new_pages) == 'all') {
      sel_exercises$type <- "p"
    } else {
      sel_exercises$type <- ""
    }
  }
  for (examinee in ex$examinees) {
    if (select_n_exercises < n) {
      i <- sample.int(n, select_n_exercises)
      if (!ex$reorder_exercises) {
        i <- sort(i)
      }
      sel_exercises <- ex$exercises[i, ]
    }
    exercises <-
      interpret_exercises(sel_exercises,
                          exam_number,
                          ex$random,
                          ex$reorder_exercises,
                          ex$delivery)
    all_exercises <-
      interpret_all_exercises(sel_exercises,
                              exam_number,
                              ex$random,
                              ex$reorder_exercises,
                              ex$delivery)

    rmarkdown::render(
      ex$rmd,
      output_format,
      output_file = paste0(out_dir, snakecase::to_snake_case(examinee)),
      encoding = encoding,
      params = list(
        exam_number = exam_number,
        exam_number_str = ex$instances[exam_number],
        examinee = examinee,
        exercises = exercises,
        all_exercises = all_exercises
      )
    )
    exam_number <- exam_number + 1
  }
  ex
}


#' Generate the support document for exam correction
#'
#' From an exam object, we can generate instances that serve as support for the
#' correction of the exam. Each instance will include the answers, if they are
#' indicated, associated with the exercises. In any case, the randomly included
#' part of the exam will be highlighted.
#'
#' To do this, we specify the folder where the documents will be generated (using
#' parameter `out_dir`), the output format (using parameter `output_format`), the
#' encoding (using parameter `encoding`), and whether we want each exercise to
#' start on a new page, include exercises until the pages are filled, or preserve
#' the definition of the exercise in this regard (using parameter `new_pages`).
#'
#' @param ex An `exam` object.
#' @param out_dir A string indicating the output folder.
#' @param output_format A vector of strings specifying the desired output formats.
#' @param encoding A string specifying the encoding.
#' @param new_pages A string with the values 'all', 'none', or 'NULL'.
#'
#' @return An `exam` object.
#'
#' @family exam definition
#'
#' @examples
#'
#' \donttest{
#' rmd <- system.file("extdata/template01.Rmd", package = "rexer")
#' exercises <- system.file("extdata/exercises.csv", package = "rexer")
#' ex <- exam(
#'   rmd = rmd,
#'   examinees = NULL,
#'   instances_num = 1,
#'   random = TRUE,
#'   reorder_exercises = TRUE
#' ) |>
#'   define_exercises_from_csv(exercises)
#' # ex |>
#' #   generate_correction_document(out_dir = tempdir())
#' }
#'
#' @export
generate_correction_document <-
  function(ex,
           out_dir,
           output_format,
           encoding,
           new_pages)
    UseMethod("generate_correction_document")

#' @rdname generate_correction_document
#' @export
generate_correction_document.exam <-
  function(ex,
           out_dir = NULL,
           output_format = "pdf_document",
           encoding = "UTF-8",
           new_pages = NULL) {
    ex_corr <- ex
    ex_corr$delivery <- FALSE
    ex_corr <-
      generate_document(ex_corr, out_dir, output_format, encoding, new_pages)
    ex
  }


#' interpret all exercise
#'
#' @param exercises A data frame, exercises.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param reorder A boolean, reorder exercises.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string.
#' @keywords internal
interpret_all_exercises <-
  function(exercises,
           exam_number,
           random,
           reorder,
           delivery) {
    if (reorder) {
      nq <- nrow(exercises)
      r <- sample(1:nq, nq, replace = FALSE)
      exercises <- exercises[r, ]
    }
    txt <- ''
    for (i in 1:nrow(exercises)) {
      exercise <-
        interpret_an_exercise(exercises[i,], exam_number, random, delivery)
      if (exercises$type[i] == 'p' & i > 1) {
        txt <- paste0(txt, '
\\newpage
')
      }
      txt <- paste0(txt, '

**', i, '.** ', exercise)
    }
    txt
  }


#' interpret exercises
#'
#' @param exercises A data frame, exercises.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param reorder A boolean, reorder exercises.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string vector.
#' @keywords internal
interpret_exercises <-
  function(exercises,
           exam_number,
           random,
           reorder,
           delivery) {
    nq <- nrow(exercises)
    if (reorder) {
      r <- sample(1:nq, nq, replace = FALSE)
    } else {
      r <- 1:nq
    }
    vq <- NULL
    for (i in r) {
      exercise <-
        interpret_an_exercise(exercises[i, ], exam_number, random, delivery)
      vq <- c(vq, exercise)
    }
    vq
  }


#' interpret a exercise.
#'
#' @param exercise A data frame, exercise.
#' @param exam_number An integer, exam sequence number
#' @param random A boolean, is random generation.
#' @param delivery A boolean, is delivery version.
#'
#' @return A string.
#' @keywords internal
interpret_an_exercise <-
  function(exercise, exam_number, random, delivery) {
    names <- names(exercise)
    base <- c("type", "statement", "image", "image_alt", "answer")
    values <- setdiff(names, base)
    others <- exercise[, values]
    others <- others[, others != '']
    txt <- exercise[, "statement"]

    avoid <- integer(0)
    if (exercise[, "image"] != '') {
      txt <- paste0(txt,
                    '

![',
                    exercise[, "image_alt"],
                    '](',
                    exercise[, "image"],
                    ')
')
      r <-
        unlist(stringr::str_extract_all(exercise[, "image"], "\\[\\[\\d\\]\\]"))
      avoid <- as.integer(stringr::str_extract(r, "\\d"))
      avoid <- avoid[!is.na(avoid)]
    }
    txt <- reorder_items(txt)

    answer <- exercise[, "answer"]
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

**Answer:**

', answer, '


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
#'
#' @keywords internal
reorder_items <- function(txt) {
  x <- stringr::str_extract_all(txt, "\\{\\{([[:print:]]+)\\}\\}")
  for (i in seq_along(x)) {
    r <- unlist(stringr::str_extract_all(x[[i]], "\\[\\[\\d\\]\\]"))
    if (length(r) > 0) {
      v <- as.integer(stringr::str_extract(r, "\\d"))
      v <- v[!is.na(v)]
      l <- length(v)
      s <- sample.int(l, l)
      w <- v[s]
      y <- x[[i]]
      for (j in seq_along(v)) {
        y <-
          gsub(paste0('[[', v[j], ']]'),
               paste0('[[XX', w[j], ']]'),
               y,
               fixed = TRUE)
      }
      y <- gsub(paste0('[[XX'), paste0('[['), y, fixed = TRUE)
      y <- gsub(paste0('{{'), '', y, fixed = TRUE)
      y <- gsub(paste0('}}'), '', y, fixed = TRUE)
      txt <- gsub(x[[i]], y, txt, fixed = TRUE)
    }
  }
  txt
}
