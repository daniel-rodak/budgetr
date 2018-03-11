CNSTsupportedBanks <- c("mbank", "idea")
CNSTtransactionCols <- c("Date", "Type", "Title", "Payee", "Amount", "Category")
CNSTtransactionTypes <- c("Date", "character", "character", "character", "numeric", "character")
CNSTtransactionTemplate <- data.frame(
  Date = as.Date(character()),
  Type = character(),
  Title = character(),
  Payee = character(),
  Amount = numeric(),
  Category = character(),
  stringsAsFactors = FALSE
)
CNSTtrOneRowTemplate <- data.frame(
  Date = Sys.Date(),
  Type = "",
  Title = "",
  Payee = "",
  Amount = 0,
  Category = "",
  stringsAsFactors = FALSE
)
CNSTdefaultBudgetCats <- c("Dochód:Regularne", "Dochód:Nieregularne",
                           "Dochód:Zyski kapitałowe", "Dochód:Zyski kapitałowe po opodatkowaniu",
                           "Wydatki:Podatki", "Wydatki:Regularne",
                           "Wydatki:Nieregularne", "Wydatki:Rozrywka",
                           "Osczędności:krótkoterminowe", "Oszczędności:długoterminowe",
                           "Oszczęsdności:Emerytura", "Systemowe")
