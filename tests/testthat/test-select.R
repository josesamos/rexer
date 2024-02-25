test_that("select", {
  r1 <- select_random(c('a', 'b', 'c'), n = 2)

  r2 <- select_random(c('a', 'b', 'c'), n = 4)

  r3 <- select_random(c('a', 'b', 'c'))

  r4 <- select_sequential(c('a', 'b', 'c'))

  r5 <- select_sequential(c('a', 'b', 'c'), n = 3)

  r6 <- select_sequential(c('a', 'b', 'c'), n = 4)

  expect_equal(length(r1), 2)

  expect_equal(length(r2), 3)

  expect_equal(length(r3), 3)

  expect_equal(r4, "a")

  expect_equal(r5, "c")

  expect_equal(r6, "a")

})
