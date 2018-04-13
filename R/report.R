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
#' @import plotly
#' @importFrom tidyr spread
#' @importFrom zoo as.yearmon as.yearqtr
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
      if (is.character(dateRange)) {
        handleCharDR(dateRange)
      } else {
        private$dateRange <- dateRange
        private$drType <- 'absolute'
      }
      private$noSys <- noSys
      data <- bdg$getTransactions(accounts, noSys)
      private$transactions <- data %>%
        dplyr::filter(Category %in% private$categories,
                      Date >= private$dateRange[1],
                      Date <= private$dateRange[2])
    },
    updateTransactions = function(bdg) {
      data <- bdg$getTransactions(private$accounts, private$noSys)
      if (private$drType != 'absolute')
        handleCharDR(private$drType)

      private$transactions <- data %>%
        dplyr::filter(Category %in% private$categories,
                      Date >= private$dateRange[1],
                      Date <= private$dateRange[2])
    },
    show = function() {
      showFun <- switch(
        private$type,
        "table" = private$showTable,
        "line" = private$showLine,
        "bar" = private$showBar,
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
    drType = character(),
    noSys = logical(),
    transactions = data.frame(),

    handleCharDR = function(dateRange) {
      stopifnot(dateRange %in% CNSTreportDateRanges)
      currDate <- Sys.Date()
      dtStart <- switch(
        dateRange,
        "30days" = currDate - 30,
        "currMonth" = as.Date(zoo::as.yearmon(currDate)),
        "prevMonth" = as.Date(zoo::as.yearmon(currDate) - 1/12),
        "last3Months" = as.Date(zoo::as.yearmon(currDate) - 1/6),
        "last6Months" = as.Date(zoo::as.yearmon(currDate) - 5/12),
        "lastYear" = as.Date(zoo::as.yearmon(currDate) - 11/12)
      )
      if (dateRange == "prevMonth")
        dtEnd <- eom(dtStart)
      else
        dtEnd <- currDate

      private$drType <- dateRange
      private$dateRange <- c(dtStart, dtEnd)
    },

    prepData = function() {
      x <- private$transactions
      x$Week <- strftime(x$Date, format = '%Y-W%W')
      x$Month <- strftime(x$Date, format = '%Y-%b') # eom(x$Date)
      x$Quarter <- format(zoo::as.yearqtr(x$Date), format = "%Y-Q%q")
      x$Year <- strftime(x$Date, format = '%Y')

      if (private$rows == 'BalanceTD' & private$cols == 'Date') {
        x <- x[, c('Date', 'Balance')]
      } else if (private$rows == 'BalanceTD' & private$cols %in% c('Week', 'Month', 'Quarter', 'Year')) {
        x <- x %>%
          dplyr::group_by(!!as.name(private$cols)) %>%
          dplyr::mutate(rn = rev(dplyr::row_number(.data$Balance))) %>%
          dplyr::ungroup() %>%
          dplyr::filter(rn == 1) %>%
          dplyr::select(dplyr::one_of(c(private$cols, 'Balance'))) %>%
          as.data.frame()
        x[, 1] <- factor(x[, 1], levels = x[, 1])
      } else if (private$rows != 'BalanceTD') {
        x <- aggregate(x$Amount, by = list(x[, private$rows], x[, private$cols]), FUN = sum)
        colnames(x) <- c(private$rows, private$cols, "Amount")
        x <- tidyr::spread(x, key = private$cols, value = "Amount", fill = 0)
        row.names(x) <- x[, 1]
        x <- x[, -1]
        x$Total <- rowSums(x)
        x <- x[order(-abs(x$Total)), ]
        x <- rbind(x, colSums(x))
        row.names(x) <- c(row.names(x)[1:(nrow(x)-1)], "Total")
      } else {
        x <- NULL
      }

      return(x)
    },

    showTable = function() {
      x <- private$prepData()
      print(x)
      invisible(x)
    },

    showLine = function() {
      x <- private$prepData()
      xvar <- colnames(x)[1]
      yvar <- colnames(x)[2]
      hovertext <- paste(x[, xvar], '<br>',
                         prettyNum(x[, yvar], ' '), 'zł')
      p <- plotly::plot_ly(
        data = x, type = 'scatter', mode = 'lines',
        x = as.formula(sprintf('~%s', xvar)),
        y = as.formula(sprintf('~%s', yvar)),
        hoverinfo = 'text',
        text = hovertext
      ) %>%
        layout(
          yaxis = list(
            exponentformat = 'SI',
            ticksuffix = ' zł'
          )
        )
      print(p)
      invisible(p)
    },

    showBar = function() {
      x <- private$prepData()
      xvar <- colnames(x)[1]
      yvar <- colnames(x)[2]
      hovertext <- paste(x[, xvar], '<br>',
                         prettyNum(x[, yvar], ' '), 'zł')
      p <- plotly::plot_ly(
        data = x, type = 'bar',
        x = as.formula(sprintf('~%s', xvar)),
        y = as.formula(sprintf('~%s', yvar)),
        hoverinfo = 'text',
        text = hovertext
      ) %>%
        layout(
          yaxis = list(
            exponentformat = 'SI',
            ticksuffix = ' zł'
          )
        )
      print(p)
      invisible(p)
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

# bdg <- budget$new('./tests/testdata/testBudget.rds')
# acc <- bdg$getAccounts()
# cats <- bdg$getCategories()
# dr <- as.Date(c("2017-10-01", "2018-04-30"))
# rep <- report$new(bdg, 'Report', 'bar', 'BalanceTD', 'Week',
#                   acc, cats, 'lastYear', FALSE)
# rep$show()