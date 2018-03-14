splitTransUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      width = 6, collapsible = TRUE,
      actionButton(ns('splitTrans'), "Podziel transakcję"),
      actionButton(ns('applySplit'), "Akceptuj"),
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

splitTrans <- function(input, output, session) {
  DF_sel <- reactiveVal()
  observeEvent(input$splitTrans, {
    req(input$dataTable_select)
    sel <- input$dataTable_select$select
    if (sel$r != sel$r2) {
      showNotification("Wybierz jedną transakcję", type = 'warning', duration = 20)
    } else {
      if (is.null(input$dataTable)) {
        dfrm <- DF()[sel$r, , drop = FALSE]
      } else {
        dfrm <- tr_to_r(input$dataTable)[sel$r, , drop = FALSE]
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

  observeEvent(input$applySplit, {
    req(input$splitTable)
    dfSplit <- hot_to_r(input$splitTable)
    if (sum(dfSplit$Kwota) != DF_sel()$Amount) {
      showNotification(paste("Suma podkategorii nie równa się", DF_sel()$Amount),
                       type = 'warning', duration = 20)
    } else {
      splitTrans <- do.call(rbind, lapply(1:nrow(dfSplit), function(x) DF_sel()))
      splitTrans$Amount <- dfSplit$Kwota
      splitTrans$Category <- dfSplit$Kategoria
      transID <- rownames(DF_sel())
      DFhot <- tr_to_r(input$dataTable)
      newData <- DFhot[rownames(DFhot) != transID, ]
      newData <- rbind(newData, splitTrans)
      newData <- newData[order(newData$Date, rownames(newData)), ]
      DF(newData)
    }
  })
}
