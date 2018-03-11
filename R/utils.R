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
  data <- lapply(seq_along(data[[1]]),function(i) sapply(data, "[[", i))
  names(data) <- hot$params$rColnames
  data <- data.frame(data, stringsAsFactors = FALSE)
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
  data$Amount <- as.double(data$Amount)
  all(colnames(data) == CNSTtransactionCols) ||
    stop("Nieprawidłowe nazwy kolumn")
  all(vapply(data, class, character(1L)) == CNSTtransactionTypes) ||
    stop("Nieprawidłowe typy kolumn")
  return(data)
}
