test_format_QIF <- function(tbl) {
  expect_equal(colnames(tbl),
               c("Date", "Type", "Title",
                 "Payee", "Amount", "Category"))
  expect_equal(vapply(tbl, class, character(1L), USE.NAMES = FALSE),
               c("Date", "character", "character",
                 "character", "numeric", "character"))
}

qif <- readQIF(file.path('..', 'testdata', 'money.qif'))
