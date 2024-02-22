test_that("common", {
  expect_equal(num_vector(1, 10),
               c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"))

  expect_equal(
    num_vector(95, 105),
    c(
      "095",
      "096",
      "097",
      "098",
      "099",
      "100",
      "101",
      "102",
      "103",
      "104",
      "105"
    )
  )

  expect_equal(name_with_nexus('a'), "a/")

  expect_equal(name_with_nexus('a/'), "a/")
})
