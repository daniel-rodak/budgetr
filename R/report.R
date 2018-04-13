# library(plotly)
# library(budgetr)
# library(dplyr)
# bdg <- budget$new('~/../Desktop/HomeBudget/HomeBudget.rds')
# data <- bdg$getTransactions(bdg$getAccounts())
#
# df.plot <- data %>% filter(ParBudgCat == 'Wydatki', eom(Date) == '2018-01-31') %>%
#   group_by(ParCat) %>% summarise(Amount = -sum(Amount)) %>%
#   arrange(-Amount) %>% as.data.frame() %>%
#   mutate(ParCat = factor(ParCat, levels = ParCat))
#
# plot_ly(data = df.plot, x = ~ParCat, y = ~Amount, type = 'bar')
# plot_ly(data = df.plot, labels = ~ParCat, values = ~Amount, type = 'pie')
#
# df.plot <- data %>% filter(ParBudgCat != 'Systemowe')
# plot_ly(data = df.plot, x = ~Date, y = ~Balance, type = 'scatter', mode = 'lines',
#         hoverinfo = "text",
#         text = ~paste(Date, "<br>", prettyNum(Balance, " "), "zł"))


#' Reference class for storing report settings and plotting it
#'
#' @examples
#' \dontrun{
#' cats <- bdg$getCategories()
#' cats <- cats[grepl("Wydatki:", names(cats))]
#' expenses <- report$new(bdg, "Wydatki", "table", "ParCat", "Month",
#'                        bdg$getAccounts(), cats,
#'                        as.Date(c("2017-10-31", "2018-03-31")))}
#' @import dplyr
#' @importFrom tidyr spread
#' @export
report <- R6::R6Class(
  classname = 'report',
  public = list(
    name = "Raport",
    initialize = function(bdg, name = "Raport",
                          type = c("table", "bar", "line"),
                          rows,
                          cols,
                          accounts, categories, dateRange,
                          noSys = TRUE) {
      self$name <- name
      private$type <- match.arg(type)
      private$rows <- match.arg(rows, unname(CNSTreportRows))
      private$cols <- match.arg(cols, unname(CNSTreportCols))
      private$accounts <- accounts
      private$categories <- categories
      private$dateRange <- dateRange
      private$noSys <- noSys
      data <- bdg$getTransactions(accounts, noSys)
      private$transactions <- data %>%
        dplyr::filter(Category %in% private$categories,
                      Date >= private$dateRange[1],
                      Date <= private$dateRange[2])
    },
    updateTransactions = function(bdg) {
      data <- bdg$getTransactions(private$accounts, private$noSys)
      private$transactions <- data %>%
        dplyr::filter(Category %in% private$categories,
                      Date >= private$dateRange[1],
                      Date <= private$dateRange[2])
    },
    show = function() {
      showFun <- switch(
        private$type,
        "table" = private$showTable,
        private$showTable)
      showFun()
    },
    reportMetadata = function() {
      ret <- data.frame(
        Name = self$name,
        Type = private$type,
        From = private$dateRange[1],
        To = private$dateRange[2],
        Rows = private$rows,
        Columns = private$cols,
        stringsAsFactors = FALSE
      )
      return(ret)
    }
  ),
  private = list(
    type = character(),
    rows = character(),
    cols = character(),
    accounts = character(),
    categories = character(),
    dateRange = as.Date(character()),
    noSys = logical(),
    transactions = data.frame(),

    prepData = function() {
      x <- private$transactions
      x$Month <- eom(x$Date)
      x <- aggregate(x$Amount, by = list(x[, private$rows], x[, private$cols]), FUN = sum)
      colnames(x) <- c(private$rows, private$cols, "Amount")
      x <- tidyr::spread(x, key = private$cols, value = "Amount", fill = 0)
      row.names(x) <- x[, 1]
      x <- x[, -1]
      x$Total <- rowSums(x)
      x <- x[order(-abs(x$Total)), ]
      x <- rbind(x, colSums(x))
      row.names(x) <- c(row.names(x)[1:(nrow(x)-1)], "Total")
      return(x)
    },

    showTable = function() {
      x <- private$prepData()
      print(x)
      invisible(x)
    }
  )
)

# bdg <- budget$new('~/../Desktop/HomeBudget/HomeBudget.rds')
# cats <- bdg$getCategories()
# cats <- cats[grepl("Wydatki:", names(cats))]
# cats <- c(cats, "Systemowe" = "[Idea - ZYSKOWNE FWN]")
# expenses <- report$new(bdg, "Wydatki", "table", "ParCat", "Month",
#                        bdg$getAccounts(), cats,
#                        as.Date(c("2017-10-01", "2018-04-30")), FALSE)
# expenses$show()

# cats <- bdg$getCategories()
# acc <- bdg$getAccounts()
# acc <- acc[!(acc %in% c("Idea - ZOŚKA PRO FB", "Idea - ZYSKOWNE FWN"))]
# net <- report$new(bdg, "Wartosc Netto", "table", "ParCat", "Month",
#                   acc, cats,
#                   as.Date(c("2017-10-01", "2018-03-31")), TRUE)

# library(dplyr)
# library(plotly)
# bdg <- budget$new('~/../Desktop/HomeBudget/HomeBudget.rds')
# acc <- bdg$getAccounts()
# acc <- acc[!(acc %in% c("Idea - ZOŚKA PRO FB", "Idea - ZYSKOWNE FWN"))]
# bdg$getTransactions(acc) -> data
# df.plot <- data %>%
#   mutate(Month = as.Date(budgetr:::eom(Date))) %>%
#   filter(Date == Month) %>% group_by(Date) %>% summarise(Balance = max(Balance))
# plot_ly(data = data, x = ~Date, y = ~Balance, type = 'scatter', mode = 'lines',
#         hoverinfo = "text",
#         text = ~paste(Date, "<br>", prettyNum(Balance, " "), "zł"))
