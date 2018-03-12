#' Function which transform handsontable object into data.frame
#'
#' This function is customized version of \code{\link[rhandsontable]{hot_to_r}}
#' suitable only for transaction data in \link{budget} R6 class
#'
#' @param hot hot table to be transformed
#'
#' @return data.frame
#' @author Daniel Rodak
#' @export
tr_to_r <- function(hot) {
  data <- hot$data
  data <- lapply(seq_along(data[[1]]),
                 function(i) sapply(data,
                                    function(j) {
                                      ifelse(is.null(j[[i]]), NA, j[[i]])
                                    }))
  names(data) <- hot$params$rColnames
  data <- data.frame(data, stringsAsFactors = FALSE)
  all(sapply(data$Date, isTruthy)) || stop("Uzupełnij daty transakcji")
  all(sapply(data$Amount, isTruthy)) || stop("Uzupełnij wysokości transakcji")
  all(sapply(data$Category, isTruthy)) || stop("Uzupełnij kategorie transakcji")
  all(sapply(data$Payee, isTruthy)) || stop("Uzupełnij płatników transakcji")
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

