test_that("exam", {
  rmd <- system.file("extdata/template01.Rmd", package = "rexer")
  ex <- exam(
    rmd = rmd,
    examinees = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_exercises = TRUE,
    select_n_exercises = NULL
  )
  ex$rmd <- basename(ex$rmd)

  ex2 <- exam(
    rmd = rmd,
    examinees = c("a", "b", "c"),
    instances_num = 1,
    random = TRUE,
    reorder_exercises = TRUE,
    select_n_exercises = NULL
  )

  txt <-
    "What are the three-letter country code (ISO 3166-1 alpha-3) for {{[[1]], [[2]] and [[3]]}}?"
  set.seed(123)
  txt <- reorder_items(txt)


  f3 <- system.file("extdata/exercises.csv", package = "rexer")
  ex3 <- define_exercises_from_csv(ex, f3)

  q1 <-
    read_exercise_csv(file = system.file("extdata/exercises.csv", package = "rexer"))
  q2 <-
    read_exercise_csv(file = system.file("extdata/exercises_image.csv", package = "rexer"))
  f <- system.file("extdata/figures", package = "rexer")
  q2$image <- gsub("xxxxx", f, q2$image, fixed = TRUE)
  q <- rbind(q1, q2)
  ex4 <- ex |>
    define_exercises(q)

  ex5 <- exam(
    rmd = rmd,
    examinees = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_exercises = TRUE,
    select_n_exercises = NULL
  ) |>
    define_exercises(q1)

  r1 <- generate_correction_document(ex5, out_dir = tempdir())

  r2 <- generate_document(ex5, out_dir = tempdir())

  r3 <- generate_document(ex5, out_dir = tempdir(), new_pages = 'all')

  r4 <- generate_document(ex5, out_dir = tempdir(), new_pages = 'none')

  ex6 <- ex5
  ex6$select_n_exercises <- 3
  r5 <- generate_document(ex6, out_dir = tempdir(), new_pages = 'none')

  ex6$select_n_exercises <- 30
  r6 <- generate_document(ex6, out_dir = tempdir(), new_pages = 'none')

  expect_equal(ex, structure(
    list(
      rmd = "template01.Rmd",
      a_n = 3,
      exercises = structure(
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
      reorder_exercises = TRUE,
      select_n_exercises = NULL,
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
    interpret_an_exercise(
      exercise = ex4$exercises[2,],
      exam_number = 5,
      random = FALSE,
      delivery = TRUE
    ),
    "What is the three-letter country code (ISO 3166-1 alpha-3) for Lithuania?"
  )

  expect_equal(
    interpret_an_exercise(
      exercise = ex4$exercises[2,],
      exam_number = 5,
      random = FALSE,
      delivery = FALSE
    ),
    "What is the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Lithuania]]**?\n\n**Answer:**\n\nLTU\n\n\n"
  )

  expect_equal({
    set.seed(123)
    interpret_an_exercise(
      exercise = ex4$exercises[2,],
      exam_number = 5,
      random = TRUE,
      delivery = FALSE
    )
  },
  "What is the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Mexico]]**?\n\n**Answer:**\n\nMEX\n\n\n")

  expect_equal({
    q <- ex4$exercises[9,]
    q$image <- "spain.png"
    interpret_an_exercise(
      exercise = q,
      exam_number = 5,
      random = FALSE,
      delivery = FALSE
    )
  },
  "What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?\n\n![Country outline.](spain.png)\n\n\n**Answer:**\n\nESP\n\n\n")

  expect_equal({
    set.seed(123)
    interpret_an_exercise(
      exercise = ex4$exercises[4, ],
      exam_number = 5,
      random = TRUE,
      delivery = FALSE
    )
  },
  "What are the three-letter country code (ISO 3166-1 alpha-3) for **[[1: Mexico]]** and **[[2: Burkina Faso]]**?")

  expect_equal({
    set.seed(123)
    interpret_an_exercise(
      exercise = ex4$exercises[4, ],
      exam_number = 5,
      random = TRUE,
      delivery = TRUE
    )
  },
  "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?")

  expect_equal({
    set.seed(123)
    interpret_exercises(
      ex5$exercises,
      exam_number = 7,
      random = FALSE,
      reorder = FALSE,
      delivery = TRUE
    )
  },
  c("What is the three-letter country code (ISO 3166-1 alpha-3) for Austria?",
    "What is the three-letter country code (ISO 3166-1 alpha-3) for Mexico?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Belize, Mexico and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Burkina Faso, Mexico and Belize?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?"
  ))

  expect_equal({
    set.seed(123)
    interpret_exercises(
      ex5$exercises,
      exam_number = 7,
      random = FALSE,
      reorder = TRUE,
      delivery = TRUE
    )
  },
  c("What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?",
    "What is the three-letter country code (ISO 3166-1 alpha-3) for Mexico?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?",
    "What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?",
    "What is the three-letter country code (ISO 3166-1 alpha-3) for Austria?"
  ))

  expect_equal({
    set.seed(123)
    interpret_all_exercises(
      ex5$exercises,
      exam_number = 7,
      random = FALSE,
      reorder = FALSE,
      delivery = TRUE
    )
  }, "\n\n**1.** What is the three-letter country code (ISO 3166-1 alpha-3) for Austria?\n\n**2.** What is the three-letter country code (ISO 3166-1 alpha-3) for Mexico?\n\n**3.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?\n\n**4.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?\n\n**5.** What are the three-letter country code (ISO 3166-1 alpha-3) for Belize, Mexico and Burkina Faso?\n\n**6.** What are the three-letter country code (ISO 3166-1 alpha-3) for Burkina Faso, Mexico and Belize?\n\n**7.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?\n\n**8.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?")

  expect_equal({
    set.seed(123)
    interpret_all_exercises(
      ex5$exercises,
      exam_number = 7,
      random = FALSE,
      reorder = TRUE,
      delivery = TRUE
    )
  }, "\n\n**1.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?\n\n**2.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?\n\n**3.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?\n\n**4.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Burkina Faso and Belize?\n\n**5.** What is the three-letter country code (ISO 3166-1 alpha-3) for Mexico?\n\n**6.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico and Burkina Faso?\n\n**7.** What are the three-letter country code (ISO 3166-1 alpha-3) for Mexico, Belize and Burkina Faso?\n\n**8.** What is the three-letter country code (ISO 3166-1 alpha-3) for Austria?")

})
