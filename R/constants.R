CNSTsupportedBanks <- c("mbank", "idea")
CNSTtransactionCols <- c("Date", "Type", "Title", "Payee", "Amount", "Category")
CNSTtransactionTypes <- c("Date", "character", "character", "character", "numeric", "character")
CNSTtransactionTemplate <- data.frame(
  Date = as.Date(character()),
  Type = character(),
  Title = character(),
  Payee = character(),
  Amount = numeric(),
  Category = character()
)
