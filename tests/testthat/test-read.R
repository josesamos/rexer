test_that("read", {

  f3 <- system.file("extdata/questions.csv", package = "rexer")
  ft3 <- read_csv(f3)

  f4 <- system.file("extdata/questions.xlsx", package = "rexer")
  ft4 <- read_excel(f4)
  ft41 <- read_excel(f4, sheet_index = 1)


  expect_equal(ft3, ft4)

  expect_equal(ft41, ft4)
})
