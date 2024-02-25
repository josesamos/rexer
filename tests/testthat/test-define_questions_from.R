test_that("define_questions_from", {

  ex <- exam(
    rmd = system.file("extdata/template01.Rmd", package = "rexer"),
    examined = NULL,
    instances_num = 1,
    random = TRUE,
    reorder_questions = TRUE,
    select_n_questions = NULL
  )

  ex <- define_a_question(
    ex,
    type = 'p',
    question = 'What is the three-letter country code (ISO 3166-1 alpha-3) for for the country represented in the figure below?',
    image = paste0(system.file("extdata/figures", package = "rexer"), "/", '[[1]]'),
    image_alt = 'Country outline.',
    answer = 'ESP<|>CHL<|>NZL<|>ITA',
    "spain.png<|>chile.png<|>new_zealand.png<|>italy.png"
  )


  expect_equal(names(ex$questions), c("type", "question", "image", "image_alt", "answer", "a_1",
                                      "a_2", "a_3"))

  expect_equal(nrow(ex$questions), 1)
})
