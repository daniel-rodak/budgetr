#' UI for splitting transaction module
#'
#' @param id module ID
#' @param accBut action button for split acceptation
#'
#' @author Daniel Rodak
#' @export
#' @rdname splitTransaction
splitTransactionUI <- function(id, accBut) {
  ns <- NS(id)
  fluidRow(
    box(width = 6, collapsible = TRUE,
      actionButton(ns('splitTrans'), "Podziel transakcję"),
      accBut,
      tableOutput(ns('selTransTable')),
      fluidRow(
        column(
          12,
          textOutput(ns('leftAmount')),
          rHandsontableOutput(ns('splitTable'), height = "20vh")
        )
      )
    )
  )
}

#' Server for splitting transaction module
#'
#' @param input input
#' @param output output
#' @param session sesion
#' @param mainTable reactive; hot table with transactions
#' @param mainTable_select reactive; select object associated with hot table
#' @param budgetCats reactive; categories for current budget
#' @param trigger reactive; counter to observe idicating whether \mainTable was
#'   added successfully
#'
#' @author Daniel Rodak
#' @export
#' @rdname splitTransaction
splitTransaction <- function(input, output, session,
                             mainTable, mainTable_select,
                             budgetCats, trigger) {
  DF_sel <- reactiveVal()
  observeEvent(input$splitTrans, {
    req(mainTable_select())
    sel <- mainTable_select()$select
    if (sel$r != sel$r2) {
      showNotification("Wybierz jedną transakcję", type = 'warning', duration = 20)
    } else {
      if (is.null(mainTable())) {
        dfrm <- DF()[sel$r, , drop = FALSE]
      } else {
        dfrm <- tr_to_r(mainTable())[sel$r, , drop = FALSE]
      }
    }
    DF_sel(dfrm)
  })

  observeEvent(input$splitTrans, {
    output$selTransTable <- renderTable(DF_sel())

    output$splitTable <- renderRHandsontable({
      req(DF_sel())
      dfrm <- data.frame(
        Kategoria = c(DF_sel()$Category, ""),
        Kwota = c(DF_sel()$Amount, 0),
        stringsAsFactors = FALSE
      )
      rhandsontable(dfrm, stretchH = "all") %>%
        hot_context_menu(allowColEdit = FALSE) %>%
        hot_col(col = "Kategoria", type = "autocomplete",
                source = budgetCats(), strict = TRUE)
    })
  })

  output$leftAmount <- renderText({
    req(input$splitTable)
    dfSplit <- hot_to_r(input$splitTable)
    unassigned <- DF_sel()$Amount - sum(dfSplit$Kwota)
    sprintf("Nieprzydzielona kwota: %0.2f zł", unassigned)
  })

  newData <- reactive({
    req(input$splitTable)
    dfSplit <- hot_to_r(input$splitTable)
    if (sum(dfSplit$Kwota) != DF_sel()$Amount) {
      showNotification(paste(enc2utf8("Suma podkategorii nie równa się"), DF_sel()$Amount),
                       type = 'warning', duration = 20)
      newData <- NULL
    } else {
      splitTrans <- do.call(rbind, lapply(1:nrow(dfSplit), function(x) DF_sel()))
      splitTrans$Amount <- dfSplit$Kwota
      splitTrans$Category <- dfSplit$Kategoria
      transID <- rownames(DF_sel())
      DFhot <- tr_to_r(mainTable())
      newData <- DFhot[rownames(DFhot) != transID, ]
      newData <- rbind(newData, splitTrans)
      newData <- newData[order(newData$Date, rownames(newData)), ]
    }
    return(newData)
  })

  observeEvent(trigger(), {
    DF_sel(NULL)
    output$splitTable <- renderRHandsontable(NULL)
  })

  return(newData)
}
