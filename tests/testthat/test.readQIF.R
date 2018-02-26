test_that("QIF file loads properly",{
  test_format_QIF(qif)
  expect_equal(nrow(qif), 9)
})

test_that("Throw error on unsupported bank", {
  expect_error(readQIF(file.path('..', 'testdata', 'invest.qif')),
               "Currently only Bank and Cash accounts are supported. Contact with")
})
