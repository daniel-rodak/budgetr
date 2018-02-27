test_format_QIF <- function(tbl) {
  expect_equal(colnames(tbl), CNSTtransactionCols)
  expect_equal(vapply(tbl, class, character(1L), USE.NAMES = FALSE), CNSTtransactionTypes)
}

qif <- readQIF(file.path('..', 'testdata', 'money.qif'))
