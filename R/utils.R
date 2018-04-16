#' Function which transform handsontable transaction into data.frame
#'
#' This function is customized version of \code{\link[rhandsontable]{hot_to_r}}
#' suitable only for transaction data in \link{budget} R6 class
#'
#' @param hot hot table to be transformed
#' @param check check if Date, Amount, Payee and Category are filled
#'
#' @return data.frame
#' @author Daniel Rodak
#' @export
tr_to_r <- function(hot, check = TRUE) {
  data <- hot$data
  data <- lapply(seq_along(data[[1]]),
                 function(i) sapply(data,
                                    function(j) {
                                      ifelse(is.null(j[[i]]), NA, j[[i]])
                                    }))
  names(data) <- hot$params$rColnames
  data <- data.frame(data, stringsAsFactors = FALSE)
  check && (all(sapply(data$Date, isTruthy)) || stop("Uzupełnij daty transakcji"))
  check && (all(sapply(data$Amount, isTruthy)) || stop("Uzupełnij wysokości transakcji"))
  check && (all(sapply(data$Payee, isTruthy)) || stop("Uzupełnij płatników transakcji"))
  check && (all(sapply(data$Category, isTruthy)) || stop("Uzupełnij kategorie transakcji"))
  data$Type[is.na(data$Type)] <- ""
  data$Title[is.na(data$Title)] <- ""
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  data$Amount <- as.double(data$Amount)
  all(colnames(data) == CNSTtransactionCols) ||
    stop("Nieprawidłowe nazwy kolumn")
  all(vapply(data, class, character(1L)) == CNSTtransactionTypes) ||
    stop("Nieprawidłowe typy kolumn")
  return(data)
}

#' Function which transform handsontable split table into data.frame
#'
#' This function is customized version of \code{\link[rhandsontable]{hot_to_r}}
#' suitable only for split data in \link{budget} R6 class
#'
#' @param hot hot table to be transformed
#' @author Daniel Rodak
#' @export
spl_to_r <- function(hot) {
  data <- hot$data
  data <- lapply(seq_along(data[[1]]),
                 function(i) sapply(data,
                                    function(j) {
                                      ifelse(is.null(j[[i]]), NA, j[[i]])
                                    }))
  names(data) <- hot$params$rColnames
  data <- data.frame(data, stringsAsFactors = FALSE)
  data$Kwota[is.na(data$Kwota)] <- 0
  data$Kategoria[is.na(data$Kategoria)] <- ""
  return(data)
}

#' Function which gives end of month for date
#' @param date date vector
#' @author Daniel Rodak
eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0, tz = "")
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}

#' Function which switches names and vector elements
#' @param x vector
#' @author Daniel Rodak
switchNames <- function(x) {
  nm <- names(x)
  names(nm) <- x
  return(nm)
}

asSys <- function(account) {
  paste0("[", account, "]")
}

namedVecToList <- function(x) {
  sapply(unique(names(x)), function(y) unname(x[names(x) == y]), simplify = FALSE)
}

ifNull <- function(x, rep) {
  ifelse(is.null(x), rep, x)
}
