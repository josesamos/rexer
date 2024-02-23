test_that("support", {

  expect_equal(vector_to_string(c('a', 'b')), "a<|>b")

  expect_equal(vector_to_string(NULL), "")

})
