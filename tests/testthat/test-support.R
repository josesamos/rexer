test_that("support", {
  f1 <- tempfile(fileext = ".csv")
  r1 <- create_exercise_csv(file = f1, sep = ',')
  f2 <- tempfile(fileext = ".csv")
  r2 <- create_exercise_csv(file = f2, sep = ';')

  ft1 <- utils::read.csv(f1)

  ft2 <- utils::read.csv2(f2)

  f3 <- system.file("extdata/exercises.csv", package = "rexer")
  ft3 <- read_exercise_csv(f3)

  f4 <- system.file("extdata/exercises.xlsx", package = "rexer")
  ft4 <- read_exercise_excel(f4)
  ft41 <- read_exercise_excel(f4, sheet_index = 1)

  f5 <- tempfile(fileext = ".xlsx")
  r5 <- create_exercise_excel(file = f5)
  ft5 <- readxl::read_excel(f5,
                            col_names = TRUE,
                            col_types = "text",
                            trim_ws = TRUE)

  file <- system.file("extdata/example.csv", package = "rexer")
  df <- read_exercise_csv(file)
  df2 <- set_pending_answers(df)

  write_exercise_csv(df2, file = tempfile(fileext = '.csv'))

  write_exercise_csv(df2, file = tempfile(fileext = '.csv'), sep = ';')


  expect_equal(vector_to_string(c('a', 'b')), "a<|>b")

  expect_equal(vector_to_string(NULL), "")

  expect_equal(
    create_exercise_data_frame(),
    structure(
      list(
        type = character(0),
        statement = character(0),
        image = character(0),
        image_alt = character(0),
        answer = character(0),
        a_1 = character(0),
        a_2 = character(0),
        a_3 = character(0)
      ),
      class = "data.frame",
      row.names = integer(0)
    )
  )

  expect_equal(ft1,
               structure(
                 list(
                   type = logical(0),
                   statement = logical(0),
                   image = logical(0),
                   image_alt = logical(0),
                   answer = logical(0),
                   a_1 = logical(0),
                   a_2 = logical(0),
                   a_3 = logical(0)
                 ),
                 class = "data.frame",
                 row.names = integer(0)
               ))

  expect_equal(ft2, ft1)

  expect_equal(ft3, ft4)

  expect_equal(ft41, ft4)

  expect_equal(ft5, structure(
    list(
      type = character(0),
      statement = character(0),
      image = character(0),
      image_alt = character(0),
      answer = character(0),
      a_1 = character(0),
      a_2 = character(0),
      a_3 = character(0)
    ),
    class = c("tbl_df",
              "tbl", "data.frame"),
    row.names = integer(0)
  ))

  expect_equal(df$answer[df$answer == '?'], c("?", "?", "?"))

  expect_equal(df2$answer[2], "1.Lithuania<|>1.Uruguay<|>1.Mexico<|>1.Gabon")

  expect_equal(df2$answer[5], "1.Lithuania, 2.Dominica, 3.St. Vincent & Grenadines<|>1.Uruguay, 2.Mali, 3.Paraguay<|>1.Mexico, 2.Burkina Faso, 3.Belize<|>1.Gabon, 2.Burundi, 3.Kyrgyzstan")

  expect_equal(df2$answer[6], "1.Lithuania, 2.Dominica, 3.St. Vincent & Grenadines<|>1.Uruguay, 2.Mali, 3.Paraguay<|>1.Mexico, 2.Burkina Faso, 3.St. Vincent & Grenadines<|>1.Gabon, 2.Dominica, 3.Paraguay")

})
