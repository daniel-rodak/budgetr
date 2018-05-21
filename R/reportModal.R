#' Modal window for editing or adding report
#'
#' @param newReport logical indicating whether this is new report
#' @param bdg objerct of class \code{budget}
#' @param metaFiller list given by report$metaFiller() to fill fields in the form
#' @author Daniel Rodak
#' @export
reportSettings <- function(newReport = TRUE, bdg, metaFiller = list()) {
  if (newReport) {
    but <- actionButton('addReportConfirm', "Dodaj")
  } else {
    but <- actionButton('editReportConfirm', "Akceptuj")
  }
  modalDialog(
    title = 'Ustawienia raportu',
    textInput("repName", "Nazwa", value = metaFiller$name),
    fluidRow(
      column(
        6,
        selectInput("repType", "Typ", choices = CNSTreportTypes,
                    selected = metaFiller$type),
        selectInput("repRows", "Wiersze", choices = CNSTreportRows,
                    selected = metaFiller$rows),
        selectInput("repCols", "Kolumny", choices = CNSTreportCols,
                    selected = metaFiller$cols)
      ),
      column(
        6,
        selectInput("repAccounts", "Konta", choices = bdg$getAccounts(),
                    multiple = TRUE, selected = metaFiller$accounts),
        selectInput("repCategories", "Kategorie",
                    choices = c(list(Wszystko = "Wszystko"),
                                namedVecToList(bdg$getCategories())),
                    multiple = TRUE, selected = metaFiller$categories),
        checkboxInput("repNoSys", "Pomiń przelewy wewnętrzne",
                      value = ifNull(metaFiller$noSys, TRUE))
      )
    ),
    checkboxInput("repCustomDate", "Zakres niestandardowy",
                  value = (ifNull(metaFiller$drType, CNSTreportDateRanges[1]) == 'absolute')),
    conditionalPanel(
      condition = "input.repCustomDate == false",
      selectInput("repDateRange", "Zakres dat", choices = CNSTreportDateRanges,
                  selected = ifelse(is.null(metaFiller$drType) || metaFiller$drType == 'absolute',
                                    CNSTreportDateRanges[1], metaFiller$drType))
    ),
    conditionalPanel(
      condition = "input.repCustomDate == true",
      dateRangeInput("repCustomDateRange", "Zakres dat",
                     start = metaFiller$dateRange[1], end = metaFiller$dateRange[2])
    ),
    footer = fluidRow(
      but,
      modalButton('Anuluj')
    )
  )
}
