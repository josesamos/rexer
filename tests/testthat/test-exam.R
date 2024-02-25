test_that("exam", {
  rmd <- system.file("extdata/template01.Rmd", package = "rexer")
  ex <- exam(
    rmd = rmd,
    examined = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_questions = TRUE,
    select_n_questions = NULL
  )
  ex$rmd <- basename(ex$rmd)

  ex2 <- exam(
    rmd = rmd,
    examined = c("a", "b", "c"),
    instances_num = 1,
    random = TRUE,
    reorder_questions = TRUE,
    select_n_questions = NULL
  )

  txt <-
    "What are the three-letter country code (ISO 3166-1 alpha-3) for {{[[1]], [[2]] and [[3]]}}?"
  set.seed(123)
  txt <- reorder_items(txt)


  f3 <- system.file("extdata/questions.csv", package = "rexer")
  ex3 <- define_questions_from_csv(ex, f3)

  q1 <-
    read_question_csv(file = system.file("extdata/questions.csv", package = "rexer"))
  q2 <-
    read_question_csv(file = system.file("extdata/questions_image.csv", package = "rexer"))
  f <- system.file("extdata/figures", package = "rexer")
  q2$image <- gsub("xxxxx", f, q2$image, fixed = TRUE)
  q <- rbind(q1, q2)
  ex4 <- ex |>
    define_questions(q)


  expect_equal(ex, structure(
    list(
      rmd = "template01.Rmd",
      a_n = 3,
      questions = structure(
        list(
          type = character(0),
          question = character(0),
          image = character(0),
          image_alt = character(0),
          answer = character(0),
          a_1 = character(0),
          a_2 = character(0),
          a_3 = character(0)
        ),
        class = "data.frame",
        row.names = integer(0)
      ),
      examined = "1",
      instances = "1",
      random = TRUE,
      reorder_questions = TRUE,
      select_n_questions = NULL,
      delivery = TRUE,
      seed = 173
    ),
    class = "exam"
  ))

  expect_equal(ex2$instances, c('1', '2', '3'))

  expect_equal(
    txt,
    "What are the three-letter country code (ISO 3166-1 alpha-3) for [[3]], [[1]] and [[2]]?"
  )

  expect_equal(
    interpret_a_question(
      question = ex4$questions[2,],
      exam_number = 5,
      random = FALSE,
      delivery = TRUE
    ),
    "What is the three-letter country code (ISO 3166-1 alpha-3) for Lithuania?"
  )

  expect_equal(
    interpret_a_question(
      question = ex4$questions[2,],
      exam_number = 5,
      random = FALSE,
      delivery = FALSE
    ),
    "What is the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Lithuania]]**?\n\n**Answer:**\n\nLTU\n\n\n"
  )

  expect_equal({
    set.seed(123)
    interpret_a_question(
      question = ex4$questions[2,],
      exam_number = 5,
      random = TRUE,
      delivery = FALSE
    )
  },
  "What is the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Mexico]]**?\n\n**Answer:**\n\nMEX\n\n\n")

  expect_equal({
    q <- ex4$questions[9,]
    q$image <- "spain.png"
    interpret_a_question(
      question = q,
      exam_number = 5,
      random = FALSE,
      delivery = FALSE
    )
  },
  "What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?\n\n![Country outline.](spain.png)\n\n\n**Answer:**\n\nESP\n\n\n")

  expect_equal({
    set.seed(123)
    interpret_a_question(
      question = ex4$questions[4, ],
      exam_number = 5,
      random = TRUE,
      delivery = FALSE
    )
  },
  "What are the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Mexico]]** and **[[2: Burkina Faso]]**?")


  expect_equal({
    set.seed(123)
    interpret_a_question(
      question = ex4$questions[4, ],
      exam_number = 5,
      random = TRUE,
      delivery = TRUE
    )
  },
  "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?")


})
