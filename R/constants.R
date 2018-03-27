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
CNSTdefaultBudgetCats <- enc2utf8(c("Dochód:Regularne", "Dochód:Nieregularne",
                                    "Dochód:Zyski kapitałowe", "Dochód:Zyski kapitałowe po opodatkowaniu",
                                    "Wydatki:Podatki", "Wydatki:Regularne",
                                    "Wydatki:Nieregularne", "Wydatki:Rozrywka",
                                    "Oszczędności:krótkoterminowe", "Oszczędności:długoterminowe",
                                    "Oszczędności:Emerytura", "Systemowe"))
CNSTreportRows <- enc2utf8(c("Nadkategorie budżetowe" = "ParBudgCat",
                             "Kategorie budżetowe" = "BudgetCategory",
                             "Nadkategorie" = "ParCat",
                             "Kategorie" = "Category",
                             "Konta" = "Account",
                             "Tygodnie" = "Week",
                             "Miesiące" = "Month"))
CNSTreportCols <- enc2utf8(c("Nadkategorie budżetowe" = "ParBudgCats",
                             "Kategorie budżetowe" = "BudgetCategory",
                             "Nadkategorie" = "ParCats",
                             "Kategorie" = "Category",
                             "Konta" = "Account",
                             "Tygodnie" = "Week",
                             "Miesiące" = "Month",
                             "Kwartały" = "Quarter",
                             "Lata" = "Year"))
CNSTreportTypes <- enc2utf8(c("Tabela" = "table",
                              "Wykres słupkowy" = "bar",
                              "Wykres liniowy" = "line"))
