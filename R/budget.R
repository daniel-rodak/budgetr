#' Reference class budget
#'
#' This class holds and manages accounts, transactions and categories. It can be
#' ititialized with special RDS file or by add* methods. It also allows to save
#' the state in RDS file. This file is a simple copy of private members.
#'
#' @docType class
#' @return object of \code{\link{R6Class}} with budget storage and interface
#'   methods
#' @format \code{\link{R6Class}} object.
#' @examples
#' myBudget <- budget$new()
#' \dontrun{
#' myBudget <- budget$new("path/to/budget.rds")}
#' @section Methods:
#' \describe{
#'   \item{\code{new(path = NULL)}}{Object creator. You can provide path to RDS file or initialize empty object}
#'   \item{\code{save(path)}}{Save budget to RDS file}
#'
#'   \item{\code{addCategory(category, budgetCat = rep(private$budgetCats[1], length(category)))}}{Add transaction category(ies) to budget along with budget category}
#'   \item{\code{deleteCategory(category)}}{Delete transaction category(ies) from budget}
#'   \item{\code{getCategories()}}{Return category vector}
#'   \item{\code{moveCategory(oldCategory, newCategory)}}{Move old category to new one}
#'
#'   \item{\code{addBudgetCategory(budgetCat)}}{Add transaction budget category(ies) to budget}
#'   \item{\code{deleteBudgetCategory(budgetCat)}}{Delete transaction budget category(ies) from budget}
#'   \item{\code{getBudgetCategories()}}{Return budget category vector}
#'
#'   \item{\code{addAccount(account, initialBalance = rep(0, length(account)))}}{Add account(s) to budget}
#'   \item{\code{deleteAccount(account)}}{Delete account(s) from budget}
#'   \item{\code{setAccountInitialBalance(account, initialBalance = rep(0, length(account)))}}{Set new account(s) initial balance(s)}
#'   \item{\code{getAccounts()}}{Return accounts vector}
#'   \item{\code{getAccountInitialBalances()}}{Return account initial balances vector}
#'   \item{\code{getAccountBalances()}}{Return account balances vector}
#'
#'   \item{\code{addTransaction(account, transaction)}}{Add transaction(s) to account}
#'   \item{\code{deleteTransaction(account, trIds)}}{Delete transaction(s) from account}
#'   \item{\code{getTransactionTable(account)}}{Get transaction table for account}
#' }
#' @author Daniel Rodak
#' @import R6
#' @export
budget <- R6::R6Class(
  classname = "budget",
  public = list(
    name = "Nowy Budżet",
    initialize = function(path = NULL) {
      if (!is.null(path)) {
        bdgt <- readRDS(path)
        private$validateBudget(bdgt)
        private$path <- bdgt$path
        private$accounts <- bdgt$accounts
        private$accInit <- bdgt$accInit
        private$accBalance <- bdgt$accBalance
        private$categories <- bdgt$categories
        private$budgetCats <- bdgt$budgetCats
        private$transactions <- bdgt$transactions
        self$name <- gsub("(.rds)$", "", basename(path))
      } else {
        private$path <- character()
        private$accounts <- character()
        private$accInit <- numeric()
        private$accBalance <- numeric()
        private$categories <- character()
        private$budgetCats <- CNSTdefaultBudgetCats
        private$transactions <- list()
      }
      return(invisible(self))
    },
    save = function(path) {
      private$path <- path
      self$name <- gsub("(.rds)$", "", basename(path))
      saveObj <- list(
        path = private$path,
        accounts = private$accounts,
        accInit = private$accInit,
        accBalance = private$accBalance,
        categories = private$categories,
        budgetCats = private$budgetCats,
        transactions = private$transactions
      )
      saveRDS(saveObj, file = path)
      return(invisible(self))
    },

    addCategory = function(category, budgetCat = rep(private$budgetCats[1], length(category))) {
      private$validateAddCategory(category, budgetCat)
      names(category) <- budgetCat
      existingCats <- intersect(category, private$categories)
      if (length(existingCats) > 0) {
        warning("Kategorie: ", paste(collapse = ", ", existingCats),
                " istnieją. Pomijam ich dodawanie.")
        category <- setdiff(category, existingCats)
      }
      private$categories <- sort(c(private$categories, category))
      return(invisible(self))
    },
    deleteCategory = function(category) {
      private$validateDeleteCategory(category)
      private$categories <- sort(setdiff(private$categories, category))
      return(invisible(self))
    },
    getCategories = function() {
      return(private$categories)
    },
    moveCategory = function(oldCategory, newCategory) {
      private$validateMoveCategory(oldCategory, newCategory)
      for(acc in private$accounts) {
        if (oldCategory %in% private$transactions[[acc]]$Category) {
          trn <- private$transactions[[acc]]
          trn$Category[trn$Category == oldCategory] <- newCategory
          private$transactions[[acc]] <- trn
        }
      }
      return(invisible(self))
    },

    addBudgetCategory = function(budgetCat) {
      private$validateAddBudgetCategory(budgetCat)
      existingCats <- intersect(budgetCat, private$budgetCats)
      if (length(existingCats) > 0) {
        warning("Kategorie budżetowe: ", paste(collapse = ", ", existingCats),
                " już istnieją. Pomijam dodawanie.")
        budgetCat <- setdiff(budgetCat, existingCats)
      }
      private$budgetCats <- sort(c(private$budgetCats, budgetCat))
      return(invisible(self))
    },
    deleteBudgetCategory = function(budgetCat) {
      private$validateDeleteBudgetCategory(budgetCat)
      private$budgetCat <- sort(setdiff(private$budgetCats, budgetCat))
      return(invisible(self))
    },
    getBudgetCategories = function() {
      return(private$budgetCats)
    },

    addAccount = function(account, initialBalance = rep(0, length(account))) {
      private$validateAddAccount(account, initialBalance)
      existingAcc <- intersect(account, private$accounts)
      if (length(existingAcc) > 0) {
        warning("Konta: ", paste(collapse = ", ", existingAcc),
                " już istnieją. Pomijam dodawanie.")
        account <- setdiff(account, existingAcc)
      }
      private$accounts <- sort(c(private$accounts, account))
      names(initialBalance) <- account
      newInit <- c(private$accInit, initialBalance)
      private$accInit <- newInit[private$accounts]
      newBalance <- c(private$accBalance, initialBalance)
      private$accBalance <- newBalance[private$accounts]
      sapply(account, function(x) {
        private$transactions[[x]] <- CNSTtransactionTemplate
      })
      private$transactions <- private$transactions[private$accounts]
      return(invisible(self))
    },
    deleteAccount = function(account) {
      private$validateDeleteAccount(account)
      notExistingAcc <- setdiff(account, private$accounts)
      if (length(notExistingAcc) > 0) {
        warning("Konta: ", paste(collapse = ", ", notExistingAcc),
                " nie istnieją. Pomijam usuwanie.")
        account <- setdiff(account, notExistingAcc)
      }
      private$accounts <- sort(setdiff(private$accounts, account))
      private$accInit <- private$accInit[names(private$accInit %in% private$accounts)]
      private$accBalance <- private$accBalance[names(private$accBalance %in% private$accounts)]
      private$transactions <- private$transactions[private$accounts]
      return(invisible(self))
    },
    setAccountInitialBalance = function(account, initialBalance = rep(0, length(account))) {
      private$validateAddAccount(account, initialBalance)
      names(initialBalance) <- account
      notExistingAcc <- setdiff(account, private$accounts)
      if (length(notExistingAcc) > 0) {
        warning("Konta: ", paste(collapse = ", ", notExistingAcc),
                " nie istnieją. Pomijam zmianę salda początkowego")
        account <- setdiff(account, notExistingAcc)
        initialBalance <- initialBalance[account]
      }
      private$accInit[account] <- initialBalance
      return(invisible(self))
    },
    getAccounts = function() {
      return(private$accounts)
    },
    getAccountInitialBalances = function() {
      return(private$accInit)
    },
    getAccountBalances = function() {
      return(private$accBalance)
    },

    addTransaction = function(account, transaction) {
      private$validateAddTransaction(account, transaction)
      private$transactions[[account]] <- rbind(private$transactions[[account]], transaction)
      private$updateAccBalance(account)
      return(invisible(self))
    },
    deleteTransaction = function(account, trIds) {
      private$validateDeleteTransaction(account, trIds)
      private$transactions[[account]] <- private$transactions[[account]][!(rn %in% trIds), ]
      private$updateAccBalance(account)
      return(invisible(self))
    },
    getTransactionTable = function(account) {
      private$validateGetTransactionTable(account)
      trn <- private$transactions[[account]]
      trn$BudgetCategory <- vapply(trn$Category,
                                   function(x) names(private$categories[private$categories == x]),
                                   FUN.VALUE = character(1L), USE.NAMES = FALSE)
      init <- private$accInit[account]
      trn <- trn[order(trn$Date), ]
      trn$Balance <- init + cumsum(trn$Amount)
      return(trn)
    }
  ),
  private = list(
    path = character(),
    accounts = character(),
    accInit = numeric(),
    accBalance = numeric(),
    categories = character(),
    budgetCats = character(),
    transactions = list(),

    updateAccBalance = function(account) {
      private$accBalance[[account]] <- private$accInit + sum(private$transactions$Amount)
    },

    validateBudget = function(x) {
      stopifnot(is.list(x))
      stopifnot(all(names(x) == c("path", "accounts", "accInit", "accBalance", "categories", "budgetCats", "transactions")))
      stopifnot(is.character(x$path))
      stopifnot(length(x$path) == 1)
      stopifnot(is.character(x$accounts))
      stopifnot(is.numeric(x$accInit))
      stopifnot(all(names(x$accInit) == x$accounts))
      stopifnot(is.numeric(x$accBalance))
      stopifnot(all(names(x$accBalance) == x$accounts))
      stopifnot(is.character(x$categories))
      stopifnot(all(names(x$categories) %in% x$budgetCats))
      stopifnot(is.character(x$budgetCats))
      stopifnot(is.list(x$transactions))
      stopifnot(all(vapply(x$transactions,is.data.frame, logical(1L))))
      stopifnot(all(vapply(x$transactions, function(y) all(colnames(y) == CNSTtransactionCols), logical(1L))))
      stopifnot(all(vapply(x$transactions, function(y) all(vapply(y, class, character(1L), USE.NAMES = FALSE) == CNSTtransactionTypes), logical(1L))))
      stopifnot(all(names(x$transactions) == x$accounts))
    },

    validateAddCategory = function(category, budgetCat) {
      is.character(category) || stop("Kategoria nie jest wektorem tekstowym")
      is.character(budgetCat) || stop("Kategoria budżetowa nie jest wektorem tekstowym")
      length(category) == length(budgetCat) || stop("Kategorie i kategorie budżetowe powinny być tej samej długości")
      all(budgetCat %in% private$budgetCats) || stop("Kategorie biznesowe: ", setdiff(budgetCat, private$budgetCats), " nie istnieją")
    },
    validateDeleteCategory = function(category) {
      is.character(category) || stop("Kategoria nie jest wektorem tekstowym")
    },
    validateMoveCategory = function(oldCategory, newCategory) {
      is.character(oldCategory) || stop("Stara kategoria nie jest wektorem tekstowym")
      is.character(newCategory) || stop("Nowa kategoria nie jest wektorem tekstowym")
      length(oldCategory) == 1 || stop("Podano więcej niż jedną starą kategorię")
      length(newCategory) == 1 || stop("Podano więcej niż jedną nową kategorię")
      oldCategory %in% private$categories || stop(oldCategory ," nie istnieje")
      newCategory %in% private$categories || stop(newCategory ," nie istnieje")
    },

    validateAddBudgetCategory = function(budgetCat) {
      is.character(budgetCat) || stop("Kategoria budżetowa nie jest wektorem tekstowym")
    },
    validateDeleteBudgetCategory = function(budgetCat) {
      is.character(budgetCat) || stop("Kategoria budżetowa nie jest wektorem tekstowym")
    },

    validateAddAccount = function(account, initialBalance) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
      is.numeric(initialBalance) || stop("Saldo nie jest wektorem numerycznym")
      length(account) == length(initialBalance) || stop("Długość wektorów kont i sald musi być taka sama. Otrzymano odpowiednio: ",
                                                        length(account), " oraz ", length(initialBalance))
    },
    validateDeleteAccount = function(account) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
    },

    validateAddTransaction = function(account, transaction) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
      is.data.frame(transaction) || stop("Transakcja musi być ramką danych")
      colnames(transaction) == CNSTtransactionCols || stop("Kolumny transakcji muszą się nazywać: ", paste(collapse = ", ", CNSTtransactionCols))
      vapply(transaction, class, character(1L), USE.NAMES = FALSE) == CNSTtransactionTypes || stop("Typy kolumn transacki muszą być: ", paste(collapse = ", ", CNSTtransactionTypes))
      length(account) == 1 || stop("Podano więcej niż jedno konto")
      account %in% private$accounts || stop(account, " nie istnieje")
    },
    validateDeleteTransaction = function(account, trIds) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
      length(account) == 1 || stop("Podano więcej niż jedno konto")
      is.numeric(trIds) || stop("Identyfikatory transakcji nie są wektorem numerycznym")
      rn <- rownames(private$transactions[[account]])
      all(trIds %in% rn) || stop("Identyfikatory transakcji nie istnieją. Brakujące identyfikatory: ", paste(collapse = ", ", setdiff(trIds, rn)))
    },
    validateGetTransactionTable = function(account) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
      length(account) == 1 || stop("Podano więcej niż jedno konto")
    }
  ),
  lock_class = TRUE
)