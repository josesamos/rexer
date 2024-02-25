test_that("define_questions_from", {

  ex <- exam(
    rmd = system.file("extdata/template01.Rmd", package = "rexer"),
    examinees = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_questions = TRUE,
    select_n_questions = NULL
  )

  ex1 <- define_a_question(
    ex,
    type = 'p',
    question = 'What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?',
    image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
    image_alt = 'Country outline.',
    answer = 'ESP<|>CHL<|>NZL<|>ITA',
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
  )

  ex2 <- define_a_question(
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


  ex3 <- define_a_question(
    ex1,
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

  f4 <- system.file("extdata/questions.csv", package = "rexer")
  ft4 <- read_question_csv(f4)
  ex4 <- define_questions(ex, ft4)

  ex5 <- define_questions(ex2, ft4)

  ex6 <- define_questions_from_csv(ex, f4)

  f7 <- system.file("extdata/questions.xlsx", package = "rexer")
  ex7 <- define_questions_from_excel(ex, f7)
  ex71 <- define_questions_from_excel(ex, f7, sheet_index = 1)



  expect_equal(names(ex1$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                      "a_2", "a_3"))

  expect_equal(nrow(ex1$questions), 1)

  expect_equal(names(ex2$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))


  expect_equal(names(ex3$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))

  expect_equal(nrow(ex3$questions), 2)

  expect_equal(names(ex4$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex4$questions), 8)

  expect_equal(names(ex5$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3", "a_4", "a_5"))

  expect_equal(nrow(ex5$questions), 9)

  expect_equal(names(ex6$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex6$questions), 8)

  expect_equal(names(ex7$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex7$questions), 8)

  expect_equal(names(ex71$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                       "a_2", "a_3"))

  expect_equal(nrow(ex71$questions), 8)


})
