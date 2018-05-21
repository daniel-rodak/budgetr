#' Function used to create mock budget
#'
#' @author Daniel Rodak
#' @export
mockBudget <- function() {
  bdg <- budget$new()
  bdg$addAccount(c("ROR", "Gotowka", "Oszczednosci"), c(2000, 300, 1000))
  bc <- bdg$getBudgetCategories()
  bdg$addCategory(c("Pensja", "Bonus", "Jedzenie", "Chemia", "Zakupy",
                    "Rachunki", "Rozrywka", "Wakacje"),
                  bc[c(1,2,6,6,7,6,7,7)])
  currDate <- Sys.Date()
  randomString <- function(n) {
    sapply(n, function(x) {
      paste(sample(letters, x, TRUE), collapse = "")
    })
  }

  set.seed(123)
  cats <- bdg$getCategories()
  trROR <- data.frame(
    Date = sort(currDate - sample(1:120, 90, TRUE)),
    Type = randomString(rep(5,90)),
    Title = randomString(rep(10,90)),
    Payee = randomString(rep(8,90)),
    Amount = round(rnorm(90, -50, 100), 2),
    stringsAsFactors = FALSE
  )
  n_pos <- sum(trROR$Amount > 0)
  trROR$Category <- ifelse(trROR$Amount > 0,
                           cats[7],
                           sample(cats[c(1,2,5,6,8,9,10,11)], 90-n_pos, TRUE, prob = c(2,2,4,7,2,2,2,3)))
  suppressWarnings(trSalary <- data.frame(
    Date = zoo::as.Date(unique(zoo::as.yearmon(trROR$Date))),
    Type = "Income",
    Title = "Salary",
    Payee = "Employer",
    Amount = 1500,
    Category = cats[4],
    stringsAsFactors = FALSE
  ))
  trROR <- rbind(trROR, trSalary)
  trROR <- trROR[order(trROR$Date, rownames(trROR)), ]

  trCash <- data.frame(
    Date = sort(currDate - sample(1:120, 10, TRUE)),
    Type = randomString(rep(5,10)),
    Title = randomString(rep(10,10)),
    Payee = randomString(rep(8,10)),
    Amount = round(rnorm(10, -30, 10), 2),
    stringsAsFactors = FALSE
  )
  n_pos <- sum(trCash$Amount > 0)
  trCash$Category <- ifelse(trCash$Amount > 0,
                            cats[7],
                            sample(cats[c(5,6,8,9,10,11)], 10-n_pos, TRUE, prob = c(5,40,1,10,2,5)))

  bdg$addTransaction(account = 'ROR', transaction = trROR, autoSys = TRUE)
  bdg$addTransaction(account = 'Gotowka', transaction = trCash, autoSys = TRUE)

  netWorth <- report$new(bdg, 'Wartosc netto',
                         'line', 'Balance', 'Month',
                         bdg$getAccounts(), bdg$getCategories(), 'lastYear')
  expenses <- report$new(bdg, 'Wydatki', 'table', 'ParCat', 'Month',
                         c("ROR", "Gotowka"), cats[c(2,5,6,8,9,10,11)],
                         'last3Months', noSys = FALSE)
  catTotal <- report$new(bdg, 'Cat total', 'bar', 'Amount', 'Category',
                         bdg$getAccounts(), bdg$getCategories(), 'prevMonth')
  bdg$addReport(netWorth)
  bdg$addReport(expenses)
  bdg$addReport(catTotal)

  return(bdg)
}
