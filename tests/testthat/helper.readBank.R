test_format <- function(tbl) {
  expect_equal(colnames(tbl), CNSTtransactionCols)
  expect_equal(vapply(tbl, class, character(1L), USE.NAMES = FALSE), CNSTtransactionTypes)
}

mbank <- readBank(file.path('..', 'testdata', 'mbank.csv'), 'mbank')
idea <- readBank(file.path('..', 'testdata', 'idea.csv'), 'idea')
