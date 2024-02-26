test_that("define_exercises_from", {

  ex <- exam(
    rmd = system.file("extdata/template01.Rmd", package = "rexer"),
    examinees = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_exercises = TRUE,
    select_n_exercises = NULL
  )

  ex1 <- define_an_exercise(
    ex,
    type = 'p',
    question = 'What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?',
    image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
    image_alt = 'Country outline.',
    answer = 'ESP<|>CHL<|>NZL<|>ITA',
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
  )

  ex2 <- define_an_exercise(
    ex,
    type = 'p',
    question = 'What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?',
    image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
    image_alt = 'Country outline.',
    answer = 'ESP<|>CHL<|>NZL<|>ITA',
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
  )


  ex3 <- define_an_exercise(
    ex1,
    type = 'p',
    exercise = 'What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?',
    image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
    image_alt = 'Country outline.',
    answer = 'ESP<|>CHL<|>NZL<|>ITA',
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png",
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
  )

  f4 <- system.file("extdata/exercises.csv", package = "rexer")
  ft4 <- read_exercise_csv(f4)
  ex4 <- define_exercises(ex, ft4)

  ex5 <- define_exercises(ex2, ft4)

  ex6 <- define_exercises_from_csv(ex, f4)

  f7 <- system.file("extdata/exercises.xlsx", package = "rexer")
  ex7 <- define_exercises_from_excel(ex, f7)
  ex71 <- define_exercises_from_excel(ex, f7, sheet_index = 1)



  expect_equal(names(ex1$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                      "a_2", "a_3"))

  expect_equal(nrow(ex1$exercises), 1)

  expect_equal(names(ex2$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))


  expect_equal(names(ex3$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))

  expect_equal(nrow(ex3$exercises), 2)

  expect_equal(names(ex4$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex4$exercises), 8)

  expect_equal(names(ex5$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))

  expect_equal(nrow(ex5$exercises), 9)

  expect_equal(names(ex6$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex6$exercises), 8)

  expect_equal(names(ex7$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex7$exercises), 8)

  expect_equal(names(ex71$exercises), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex71$exercises), 8)


})
