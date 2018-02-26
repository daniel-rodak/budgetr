test_that("QIF file loads properly",{
  test_format_QIF(qif)
  expect_equal(nrow(qif), 9)
})
