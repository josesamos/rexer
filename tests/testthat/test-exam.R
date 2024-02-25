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
})
