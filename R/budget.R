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
#'   \item{\code{updateSystemCategories()}}{Set system categories to valid values}
#'
#'   \item{\code{addBudgetCategory(budgetCat)}}{Add transaction budget category(ies) to budget}
#'   \item{\code{deleteBudgetCategory(budgetCat)}}{Delete transaction budget category(ies) from budget}
#'   \item{\code{getBudgetCategories()}}{Return budget category vector}
#'
#'   \item{\code{addAccount(account, initialBalance = rep(0, length(account)))}}{Add account(s) to budget}
#'   \item{\code{deleteAccount(account)}}{Delete account(s) from budget}
#'   \item{\code{renameAccount(account, newName)}}{Rename account}
#'   \item{\code{setAccountInitialBalance(account, initialBalance = rep(0, length(account)))}}{Set new account(s) initial balance(s)}
#'   \item{\code{getAccounts()}}{Return accounts vector}
#'   \item{\code{getAccountInitialBalances()}}{Return account initial balances vector}
#'   \item{\code{getAccountBalances()}}{Return account balances vector}
#'
#'   \item{\code{addTransaction(account, transaction, autoSys = TRUE)}}{Add transaction(s) to account}
#'   \item{\code{deleteTransaction(account, trIds, autoSys = TRUE)}}{Delete transaction(s) from account}
#'   \item{\code{getTransactionTable(account)}}{Get transaction table for account}
#'
#'   \item{\code{addReport(report)}}{Add report to budget}
#'   \item{\code{deleteReport(name)}}{Delete report from budget}
#'   \item{\code{getReport(name)}}{Getreport}
#' }
#' @author Daniel Rodak
#' @import R6
#' @importFrom stringi stri_split_fixed
#' @export
budget <- R6::R6Class(
  classname = "budget",
  public = list(
    name = enc2utf8("Nowy Budżet"),
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
        private$reports <- bdgt$reports
        self$name <- gsub("(.rds)$", "", basename(path))
      } else {
        private$path <- character()
        private$accounts <- character()
        private$accInit <- numeric()
        private$accBalance <- numeric()
        private$categories <- character()
        private$budgetCats <- CNSTdefaultBudgetCats
        private$transactions <- list()
        private$reports <- list()
      }
      return(invisible(self))
    },
    save = function(path = NULL) {
      if (!is.null(path)) {
        private$path <- path
      }
      length(private$path) == 1 || stop(enc2utf8("Brakuje ścieżki zapisu"))
      self$name <- gsub("(.rds)$", "", basename(private$path))
      saveObj <- list(
        path = private$path,
        accounts = private$accounts,
        accInit = private$accInit,
        accBalance = private$accBalance,
        categories = private$categories,
        budgetCats = private$budgetCats,
        transactions = private$transactions,
        reports = private$reports
      )
      saveRDS(saveObj, file = private$path)
      return(invisible(self))
    },

    addCategory = function(category, budgetCat = rep(private$budgetCats[1], length(category))) {
      private$validateAddCategory(enc2utf8(category), enc2utf8(budgetCat))
      category <- enc2utf8(category)
      budgetCat <- enc2utf8(budgetCat)
      names(category) <- budgetCat
      existingCats <- intersect(category, private$categories)
      if (length(existingCats) > 0) {
        warning(enc2utf8("Kategorie: "), paste(collapse = ", ", existingCats),
                enc2utf8(" istnieją. Pomijam ich dodawanie."))
        category <- setdiff(category, existingCats)
      }
      private$categories <- sort(c(private$categories, category))
      return(invisible(self))
    },
    deleteCategory = function(category) {
      private$validateDeleteCategory(enc2utf8(category))
      category <- enc2utf8(category)
      private$categories <- sort(private$categories[!(private$categories %in% category)])
      return(invisible(self))
    },
    getCategories = function() {
      return(private$categories)
    },
    moveCategory = function(oldCategory, newCategory) {
      private$validateMoveCategory(enc2utf8(oldCategory), enc2utf8(newCategory))
      oldCategory <- enc2utf8(oldCategory)
      newCategory <- enc2utf8(newCategory)
      for(acc in private$accounts) {
        if (oldCategory %in% private$transactions[[acc]]$Category) {
          trn <- private$transactions[[acc]]
          trn$Category[trn$Category == oldCategory] <- newCategory
          private$transactions[[acc]] <- trn
        }
      }
      return(invisible(self))
    },
    updateSystemCategories = function() {
      private$validateUpdateSystemCategories()
      currSysCats <- private$categories[names(private$categories) == enc2utf8("Systemowe")]
      if (length(currSysCats) > 0) {
        self$deleteCategory(unname(currSysCats))
      }
      if (length(private$accounts) > 0) {
        sysCats <- asSys(private$accounts)
        self$addCategory(sysCats, rep(enc2utf8("Systemowe"), length(private$accounts)))
      }
    },

    addBudgetCategory = function(budgetCat) {
      private$validateAddBudgetCategory(enc2utf8(budgetCat))
      budgetCat <- enc2utf8(budgetCat)
      existingCats <- intersect(budgetCat, private$budgetCats)
      if (length(existingCats) > 0) {
        warning(enc2utf8("Kategorie budżetowe: "), paste(collapse = ", ", existingCats),
                enc2utf8(" już istnieją. Pomijam dodawanie."))
        budgetCat <- setdiff(budgetCat, existingCats)
      }
      private$budgetCats <- sort(c(private$budgetCats, budgetCat))
      return(invisible(self))
    },
    deleteBudgetCategory = function(budgetCat) {
      private$validateDeleteBudgetCategory(enc2utf8(budgetCat))
      budgetCat <- enc2utf8(budgetCat)
      private$budgetCats <- sort(setdiff(private$budgetCats, budgetCat))
      return(invisible(self))
    },
    getBudgetCategories = function() {
      return(private$budgetCats)
    },

    addAccount = function(account, initialBalance = rep(0, length(account))) {
      private$validateAddAccount(enc2utf8(account), initialBalance)
      account <- enc2utf8(account)
      existingAcc <- intersect(account, private$accounts)
      if (length(existingAcc) > 0) {
        warning(enc2utf8("Konta: "), paste(collapse = ", ", existingAcc),
                enc2utf8(" już istnieją. Pomijam dodawanie."))
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
      self$updateSystemCategories()
      return(invisible(self))
    },
    deleteAccount = function(account) {
      private$validateDeleteAccount(enc2utf8(account))
      account <- enc2utf8(account)
      notExistingAcc <- setdiff(account, private$accounts)
      if (length(notExistingAcc) > 0) {
        warning(enc2utf8("Konta: "), paste(collapse = ", ", notExistingAcc),
                enc2utf8(" nie istnieją. Pomijam usuwanie."))
        account <- setdiff(account, notExistingAcc)
      }
      private$accounts <- sort(setdiff(private$accounts, account))
      private$accInit <- private$accInit[private$accounts]
      private$accBalance <- private$accBalance[private$accounts]
      private$transactions <- private$transactions[private$accounts]
      self$updateSystemCategories()
      return(invisible(self))
    },
    renameAccount = function(account, newName) {
      private$validateRenameAccount(enc2utf8(account), enc2utf8(newName))
      account <- enc2utf8(account)
      newName <- enc2utf8(newName)
      private$accounts[private$accounts == account] <- newName
      private$accounts <- sort(private$accounts)
      names(private$accInit)[names(private$accInit) == account] <- newName
      private$accInit <- private$accInit[private$accounts]
      names(private$accBalance)[names(private$accBalance) == account] <- newName
      private$accBalance <- private$accBalance[private$accounts]
      names(private$transactions)[names(private$transactions) == account] <- newName
      private$transactions <- private$transactions[private$accounts]
      self$addCategory(asSys(newName), enc2utf8("Systemowe"))
      self$moveCategory(asSys(account), asSys(newName))
      self$updateSystemCategories()
    },
    setAccountInitialBalance = function(account, initialBalance = rep(0, length(account))) {
      private$validateAddAccount(enc2utf8(account), initialBalance)
      account <- enc2utf8(account)
      names(initialBalance) <- account
      notExistingAcc <- setdiff(account, private$accounts)
      if (length(notExistingAcc) > 0) {
        warning(enc2utf8("Konta: "), paste(collapse = ", ", notExistingAcc),
                enc2utf8(" nie istnieją. Pomijam zmianę salda początkowego"))
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

    addTransaction = function(account, transaction, autoSys = TRUE) {
      private$validateAddTransaction(enc2utf8(account), transaction)
      account <- enc2utf8(account)
      # handle system categories
      if (autoSys) {
        sysCats <- private$categories[names(private$categories) == enc2utf8('Systemowe')]
        if (length(sysCats) > 0) {
          sysTrans <- transaction[transaction$Category %in% sysCats, , drop = FALSE]
          if (nrow(sysTrans) > 0) {
            targetAcc <- gsub("^\\[|\\]$", "", unique(sysTrans$Category))
            if (account %in% targetAcc) {
              stop(enc2utf8("Nieprawidłowy przelew na to samo konto"))
            }
            for(acc in targetAcc) {
              targetTrans <- sysTrans[sysTrans$Category == asSys(acc), ]
              targetTrans$Amount <- -targetTrans$Amount
              targetTrans$Category <- asSys(account)
              private$transactions[[acc]] <- rbind(private$transactions[[acc]], targetTrans)
              private$updateAccBalance(acc)
            }
          }
        }
      }

      private$transactions[[account]] <- rbind(private$transactions[[account]], transaction)
      private$updateAccBalance(account)
      return(invisible(self))
    },
    deleteTransaction = function(account, trIds, autoSys = TRUE) {
      private$validateDeleteTransaction(enc2utf8(account), trIds)
      account <- enc2utf8(account)
      rn <- rownames(private$transactions[[account]])
      # handle system categories
      # TODO: faster implementation without loop over rows
      if (autoSys) {
        sysCats <- private$categories[names(private$categories) == enc2utf8('Systemowe')]
        if (length(sysCats) > 0) {
          sysTrans <- private$transactions[[account]][(rn %in% trIds) & (private$transactions[[account]]$Category %in% sysCats), , drop = FALSE]
          if (nrow(sysTrans) > 0) {
            for (r in 1:nrow(sysTrans)) {
              row <- sysTrans[r, , drop = FALSE]
              targetAcc <- gsub("^\\[|\\]$", "", row$Category)
              targetSysTrans <- private$transactions[[targetAcc]]
              targetRn <- rownames(targetSysTrans)
              targetTrans <- targetSysTrans[(targetSysTrans$Date == row$Date) &
                                              (targetSysTrans$Type == row$Type) &
                                              (targetSysTrans$Title == row$Title) &
                                              (targetSysTrans$Payee == row$Payee) &
                                              (targetSysTrans$Amount == -row$Amount) &
                                              (targetSysTrans$Category == asSys(account)), , drop = FALSE]
              if (nrow(targetTrans) > 0) {
                private$transactions[[targetAcc]] <- private$transactions[[targetAcc]][!(targetRn == rownames(targetTrans)[1]), ]
                private$updateAccBalance(targetAcc)
              }
            }
          }
        }
      }

      private$transactions[[account]] <- private$transactions[[account]][!(rn %in% trIds), ]
      private$updateAccBalance(account)
      return(invisible(self))
    },
    getTransactionTable = function(account) {
      private$validateGetTransactionTable(enc2utf8(account))
      account <- enc2utf8(account)
      trn <- private$transactions[[account]]
      trn$BudgetCategory <- vapply(trn$Category,
                                   function(x) names(private$categories[private$categories == x]),
                                   FUN.VALUE = character(1L), USE.NAMES = FALSE)
      init <- private$accInit[account]
      trn <- trn[order(trn$Date, row.names(trn)), ]
      trn$Balance <- init + cumsum(trn$Amount)
      return(trn)
    },
    getTransactions = function(accounts, noSys = TRUE) {
      ret <- do.call(rbind, lapply(accounts, function(x) {
          dfr <- self$getTransactionTable(x)
          cbind(Account = x, dfr)
        }))
      if (noSys) {
        combs <- combn(accounts, 2)
        for (i in 1:ncol(combs)) {
          comb <- combs[, i]
          ret <- ret[!(ret$Account == comb[1] & ret$Category == asSys(comb[2])), ]
          ret <- ret[!(ret$Account == comb[2] & ret$Category == asSys(comb[1])), ]
        }
      }
      init <- sum(private$accInit[accounts])
      ret <- ret[order(ret$Date, row.names(ret)), ]
      ret$Balance <- init + cumsum(ret$Amount)
      ret$ParCat <- stringi::stri_split_fixed(ret$Category, ":", simplify = TRUE)[, 1]
      ret$ParBudgCat <- stringi::stri_split_fixed(ret$BudgetCategory, ":", simplify = TRUE)[, 1]
      return(ret)
    },

    addReport = function(report) {
      private$validateAddReport(report)
      addRep <- list(report$metaFiller())
      names(addRep) <- addRep[[1]]$name
      private$reports <- c(private$reports, addRep)
      return(invisible(self))
    },
    deleteReport = function(name) {
      private$validateDeleteReport(name)
      reps <- setdiff(names(private$reports), name)
      private$reports <- private$reports[reps]
      return(invisible(self))
    },
    listReports = function() {
      ret <- do.call(rbind, lapply(private$reports, function(x) {
        data.frame(
          Name = x$name,
          Type = x$type,
          From = x$dateRange[1],
          To = x$dateRange[2],
          Rows = x$rows,
          Columns = x$cols,
          stringsAsFactors = FALSE
        )
      }))
      if (is.data.frame(ret) && nrow(ret) > 0) {
        ret$Type <- switchNames(CNSTreportTypes)[ret$Type]
        ret$Rows <- switchNames(CNSTreportRows)[ret$Rows]
        ret$Columns <- switchNames(CNSTreportCols)[ret$Columns]
      } else {
        ret <- NULL
      }
      return(ret)
    },
    updateReports = function() {
      # lapply(private$reports, function(x) x$updateTransactions(self))
      # return(invisible(self))
    },
    getReport = function(name) {
      private$validateGetReport(name)
      mf <- private$reports[[name]]
      rep <- report$new(self, mf$name, mf$type, mf$rows, mf$cols,
                        mf$accounts, mf$categories, mf$dateRange, mf$noSys)
      return(rep)
    },
    setReportField = function(report, field, value) {
      private$validateSetReportField(report, field, value)
      if (field == 'name') {
        private$reports[[report]]$name <- value
        private$reports[[value]] <- private$reports[[report]]
        private$reports[[report]] <- NULL
      } else if (field == 'type') {
        private$reports[[report]]$type <- value
      } else if (field == 'rows') {
        private$reports[[report]]$rows <- value
      } else if (field == 'cols') {
        private$reports[[report]]$cols <- value
      } else if (field == 'accounts') {
        private$reports[[report]]$accounts <- value
      } else if (field == 'categories') {
        private$reports[[report]]$categories <- value
      } else if (field == 'dateRange') {
        if (is.character(value)) {
          ret <- parseCharDR(value)
          private$reports[[report]]$dateRange <- ret$dateRange
          private$reports[[report]]$drType <- ret$drType
        } else {
          private$reports[[report]]$dateRange <- ret$dateRange
          private$reports[[report]]$drType <- 'absolute'
        }
      } else if (field == 'noSys') {
        private$reports[[report]]$noSys <- value
      }
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
    reports = list(),

    updateAccBalance = function(account) {
      private$accBalance[[account]] <- private$accInit[[account]] + sum(private$transactions[[account]]$Amount)
    },

    validateBudget = function(x) {
      stopifnot(is.list(x))
      stopifnot(all(names(x) == c("path", "accounts", "accInit", "accBalance", "categories", "budgetCats", "transactions", "reports")))
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
      stopifnot(all(vapply(x$reports, function(x) identical(class(x), c("report", "R6")), logical(1L))))
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
    validateUpdateSystemCategories = function() {
      if (!("Systemowe" %in% private$budgetCats))
        self$addBudgetCategory("Systemowe")
    },

    validateAddBudgetCategory = function(budgetCat) {
      is.character(budgetCat) || stop("Kategoria budżetowa nie jest wektorem tekstowym")
    },
    validateDeleteBudgetCategory = function(budgetCat) {
      is.character(budgetCat) || stop("Kategoria budżetowa nie jest wektorem tekstowym")
      !(budgetCat %in% names(private$categories)) || stop("Przed usnięciem kategorii budżetowej przenieś wszystkie kategorie do niej należące")
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
    validateRenameAccount = function(account, newName) {
      is.character(account) || stop("Stare konto nie jest wektorem tekstowym")
      is.character(newName) || stop("Nowa nazwa konta nie jest wektorem tekstowym")
      length(account) == 1 || stop("Podano więcej niż jedno konto")
      length(newName) == 1 || stop("Podano więcej niż jedną nową nazwę konta")
      account %in% private$accounts || stop(account ," nie istnieje")
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
      is.character(trIds) || stop("Identyfikatory transakcji nie są wektorem nazw")
      rn <- rownames(private$transactions[[account]])
      all(trIds %in% rn) || stop("Identyfikatory transakcji nie istnieją. Brakujące identyfikatory: ", paste(collapse = ", ", setdiff(trIds, rn)))
    },
    validateGetTransactionTable = function(account) {
      is.character(account) || stop("Konto nie jest wektorem tekstowym")
      length(account) == 1 || stop("Podano więcej niż jedno konto")
    },
    validateAddReport = function(report) {
      identical(class(report), c("report", "R6")) || stop("Podany obiekt nie jest klasy 'report'")
      name <- report$name
      if (name %in% names(private$reports)) {
        stop("Raport ", name, " istnieje")
      }
    },
    validateDeleteReport = function(name) {
      name %in% names(private$reports) || stop("Raport ", name, " nie istnieje")
      length(name) == 1 || stop("Podano więcej niż jeden raport")
    },
    validateGetReport = function(name) {
      name %in% names(private$reports) || stop("Raport ", name, " nie istnieje")
      length(name) == 1 || stop("Podano więcej niż jeden raport")
    },
    validateSetReportField = function(report, field, value) {
      report %in% names(private$reports) || stop("Raport ", report, " nie istnieje")
      length(report) == 1 || stop("Podano więcej niż jeden raport")
      field %in% c('name', 'type', 'rows', 'cols', 'accounts',
                   'categories', 'dateRange', 'noSys') ||
        stop("Niewłaściwe pole ", field)
    }
  ),
  lock_class = TRUE
)

