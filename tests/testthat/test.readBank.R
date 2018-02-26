test_that("mBank loads properly",{
  test_format(mbank)
  expect_equal(nrow(mbank), 11)
})

test_that("Idea Bank loads properly",{
  test_format(idea)
  expect_equal(nrow(idea), 7)
})

test_that("Throw error on unsupported bank", {
  expect_error(readBank("foo.csv", "bar"),
               "bar is not supported. Contact with")
})
