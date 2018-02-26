test_format <- function(tbl) {
  expect_equal(colnames(tbl),
               c("Date", "Type", "Title",
                 "Payee", "Amount"))
  expect_equal(vapply(tbl, class, character(1L), USE.NAMES = FALSE),
               c("Date", "character", "character",
                 "character", "numeric"))
}

mbank <- readBank(file.path('..', 'testdata', 'mbank.csv'), 'mbank')
idea <- readBank(file.path('..', 'testdata', 'idea.csv'), 'idea')
