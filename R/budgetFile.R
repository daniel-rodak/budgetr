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
#'   \item{\code{addCategory(category)}}{Add transaction category(ies) to budget}
#'   \item{\code{deleteCategory(category)}}{Delete transaction category(ies) from budget}
#'   \item{\code{getCategories()}}{Return category vector}
#'   \item{\code{moveCategory(oldCategory, newCategory)}}{Move old category to new one}
#'
#'   \item{\code{addAccount(account)}}{Add account(s) to budget}
#'   \item{\code{deleteAccount(account)}}{Delete account(s) from budget}
#'   \item{\code{getAccounts()}}{Return accounts vector}
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
        budget <- readRDS(path)
        validateBudget(budget)
        private$path <- budget$path
        private$accounts <- budget$accounts
        private$accInit <- budget$accInit
        private$categories <- budget$categories
        private$transactions <- budget$transactions
        self$name <- gsub("(.rds)$", "", basename(path))
      } else {
        private$path <- character()
        private$accounts <- character()
        private$accInit <- numeric()
        private$categories <- character()
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
        categories = private$categories,
        transactions = private$transactions
      )
      saveRDS(saveObj, file = path)
      return(invisible(self))
    },

    addCategory = function(category) {
      is.character(category) || stop("Provided category is not a character")
      existingCats <- intersect(category, private$categories)
      if (length(existingCats) > 0) {
        warning("Categories: ", paste(collapse = ", ", existingCats),
                " already exist. Skipping their addition.")
        category <- setdiff(category, existingCats)
      }
      private$categories <- sort(c(private$categories, category))
      return(invisible(self))
    },
    deleteCategory = function(category) {
      is.character(category) || stop("Provided category is not a character")
      private$categories <- sort(setdiff(private$categories, category))
      return(invisible(self))
    },
    getCategories = function() {
      return(private$categories)
    },
    moveCategory = function(oldCategory, newCategory) {
      is.character(oldCategory) || stop("Provided oldCategory is not a character")
      is.character(newCategory) || stop("Provided newCategory is not a character")
      length(oldCategory) == 1 || stop("Provided more than one oldCategory")
      length(newCategory) == 1 || stop("Provided more than one newCategory")
      oldCategory %in% private$categories || stop(oldCategory ," do not exist")
      newCategory %in% private$categories || stop(newCategory ," do not exist")

      for(acc in private$accounts) {
        if (oldCategory %in% private$transactions[[acc]]$Category) {
          trn <- private$transactions[[acc]]
          trn$Category[trn$Category == oldCategory] <- newCategory
          private$transactions[[acc]] <- trn
        }
      }
      return(invisible(self))
    },

    addAccount = function(account, initialBalance = rep(0, length(account))) {
      is.character(account) || stop("Provided account is not a character")
      length(account) == length(initialBalance) || stop("You must provide same number of accounts and account initial balances")
      existingAcc <- intersect(account, private$accounts)
      if (length(existingAcc) > 0) {
        warning("Accounts: ", paste(collapse = ", ", existingAcc),
                " already exist. Skipping their addition.")
        account <- setdiff(account, existingAcc)
      }
      private$accounts <- sort(c(private$accounts, account))
      names(initialBalance) <- account
      newInit <- c(private$accInit, initialBalance)
      private$accInit <- newInit[order(names(newInit))]
      sapply(account, function(x) {
        private$transactions[[x]] <- CNSTtransactionTemplate
      })
      return(invisible(self))
    },
    deleteAccount = function(account) {
      is.character(account) || stop("Provided account is not a character")
      notExistingAcc <- setdiff(account, private$accounts)
      if (length(notExistingAcc) > 0) {
        warning("Accounts: ", paste(collapse = ", ", notExistingAcc),
                " do not exist. Skipping their deletion")
        account <- setdiff(account, notExistingAcc)
      }
      private$accounts <- sort(setdiff(private$accounts, account))
      private$accInit <- private$accInit[names(private$accInit %in% private$accounts)]
      sapply(account, function(x) {
        private$transactions[[x]] <- NULL
      })
      return(invisible(self))
    },
    getAccounts = function() {
      return(private$accounts)
    },

    addTransaction = function(account, transaction) {
      is.character(account) || stop("Provided account is not a character")
      is.data.frame(transaction) || stop("transaction must be a data.frame")
      colnames(transaction) == CNSTtransactionCols || stop("transaction column names must be: ", paste(collapse = ", ", CNSTtransactionCols))
      vapply(transaction, class, character(1L), USE.NAMES = FALSE) == CNSTtransactionTypes || stop("transaction column types must be: ", paste(collapse = ", ", CNSTtransactionTypes))
      length(account) == 1 || stop("You must provide only one account")
      account %in% private$accounts || stop(account, " is not a valid account. You can add it by budget$addAccount")
      private$transactions[[account]] <- rbind(private$transactions[[account]], transaction)
      return(invisible(self))
    },
    deleteTransaction = function(account, trIds) {
      is.character(account) || stop("Provided account is not a character")
      length(account) == 1 || stop("You must provide only one account")
      is.character(trIds) || stop("Provided transaction IDs are not a character vector")
      rn <- rownames(private$transactions[[account]])
      all(trIds %in% rn) || stop("Provided transactions IDs do not exist in transaction table. Non existant IDs: ", paste(collapse = ", ", setdiff(trIds, rn)))
      private$transactions[[account]] <- private$transactions[[account]][!(rn %in% trIds), ]
      return(invisible(self))
    },
    getTransactionTable = function(account) {
      is.character(account) || stop("Provided account is not a character")
      length(account) == 1 || stop("You must provide only one account")
      trn <- private$transactions[[account]]
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
    categories = character(),
    transactions = list()
  ),
  lock_class = TRUE
)

validateBudget <- function(x) {
  stopifnot(is.list(x))
  stopifnot(names(x) == c("path", "accounts", "accInit", "categories", "transactions"))
  stopifnot(is.character(x$path))
  stopifnot(length(x$path) == 1)
  stopifnot(is.character(x$accounts))
  stopifnot(is.numeric(x$accInit))
  stopifnot(names(x$accInit) == x$accounts)
  stopifnot(is.character(x$categories))
  stopifnot(is.list(x$transactions))
  stopifnot(all(vapply(x$transactions,is.data.frame, logical(1L))))
  stopifnot(all(vapply(x$transactions, function(y) all(colnames(y) == CNSTtransactionCols), logical(1L))))
  stopifnot(all(vapply(x$transactions, function(y) all(vapply(y, class, character(1L), USE.NAMES = FALSE) == CNSTtransactionTypes), logical(1L))))
  stopifnot(names(x$transactions) == x$accounts)
}
