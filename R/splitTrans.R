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
    ),
    box(width = 6, collapsible = TRUE,
      fluidRow(
        column(10, verbatimTextOutput(ns('res'), placeholder = TRUE)),
        column(2, actionButton(ns('back'), '<x'))
      ),
      fluidRow(
        column(3, actionButton(ns('AC'), 'AC')),
        column(3, actionButton(ns('pm'), '+/-')),
        column(3, actionButton(ns('percent'), '%')),
        column(3, actionButton(ns('divide'), '/'))
      ),
      fluidRow(
        column(3, actionButton(ns('d7'), '7')),
        column(3, actionButton(ns('d8'), '8')),
        column(3, actionButton(ns('d9'), '9')),
        column(3, actionButton(ns('times'), '*'))
      ),
      fluidRow(
        column(3, actionButton(ns('d4'), '4')),
        column(3, actionButton(ns('d5'), '5')),
        column(3, actionButton(ns('d6'), '6')),
        column(3, actionButton(ns('minus'), '-'))
      ),
      fluidRow(
        column(3, actionButton(ns('d1'), '1')),
        column(3, actionButton(ns('d2'), '2')),
        column(3, actionButton(ns('d3'), '3')),
        column(3, actionButton(ns('plus'), '+'))
      ),
      fluidRow(
        column(6, actionButton(ns('d0'), '0')),
        column(3, actionButton(ns('comma'), ',')),
        column(3, actionButton(ns('eq'), '='))
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
#' @param trigger reactive; counter to observe idicating whether \code{mainTable}
#'  was added successfully
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
        dfrm <- tr_to_r(mainTable(), check = FALSE)[sel$r, , drop = FALSE]
        if (!(all(sapply(dfrm$Amount, isTruthy))))
          dfrm <- NULL
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
    dfSplit <- spl_to_r(input$splitTable)
    unassigned <- DF_sel()$Amount - sum(dfSplit$Kwota)
    sprintf("Nieprzydzielona kwota: %0.2f zł", unassigned)
  })

  newData <- reactive({
    req(input$splitTable)
    dfSplit <- spl_to_r(input$splitTable)
    if (sum(dfSplit$Kwota) != DF_sel()$Amount) {
      showNotification(paste(enc2utf8("Suma podkategorii nie równa się"), DF_sel()$Amount),
                       type = 'warning', duration = 20)
      newData <- NULL
    } else {
      splitTrans <- do.call(rbind, lapply(1:nrow(dfSplit), function(x) DF_sel()))
      splitTrans$Amount <- dfSplit$Kwota
      splitTrans$Category <- dfSplit$Kategoria
      transID <- rownames(DF_sel())
      DFhot <- tr_to_r(mainTable(), check = FALSE)
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

# Calculator --------------------------------------------------------------

  display <- reactiveVal(value = "")
  firstVal <- reactiveVal(value = 0)
  secondVal <- reactiveVal(value = 0)
  operator <- reactiveVal(value = getOperatorFunction("+"))

  observeEvent(input$d0, {display(paste0(display(), 0))})
  observeEvent(input$d1, {display(paste0(display(), 1))})
  observeEvent(input$d2, {display(paste0(display(), 2))})
  observeEvent(input$d3, {display(paste0(display(), 3))})
  observeEvent(input$d4, {display(paste0(display(), 4))})
  observeEvent(input$d5, {display(paste0(display(), 5))})
  observeEvent(input$d6, {display(paste0(display(), 6))})
  observeEvent(input$d7, {display(paste0(display(), 7))})
  observeEvent(input$d8, {display(paste0(display(), 8))})
  observeEvent(input$d9, {display(paste0(display(), 9))})
  observeEvent(input$comma, {
    if (!grepl(",", display()))
      display(paste0(display(), ","))
  })
  observeEvent(input$plus, {
    firstVal(toNumber(display()))
    display("")
    operator(getOperatorFunction("+"))
  })
  observeEvent(input$minus, {
    firstVal(toNumber(display()))
    display("")
    operator(getOperatorFunction("-"))
  })
  observeEvent(input$times, {
    firstVal(toNumber(display()))
    display("")
    operator(getOperatorFunction("*"))
  })
  observeEvent(input$divide, {
    firstVal(toNumber(display()))
    display("")
    operator(getOperatorFunction("/"))
  })
  observeEvent(input$eq, {
    secondVal(toNumber(display()))
    result <- toChar(operator()(firstVal(), secondVal()))
    display(result)
  })
  observeEvent(input$AC, {
    display("")
    firstVal(0)
    secondVal(0)
    operator(getOperatorFunction("+"))
  })
  observeEvent(input$back, {
    req(display())
    display(gsub(".$", "", display()))
  })
  observeEvent(input$pm, {
    req(display())
    display(toChar(-toNumber(display())))
  })
  observeEvent(input$percent, {
    req(display())
    display(toChar(toNumber(display()) / 100.0))
  })

  output$res <- renderText(display())

  return(newData)
}

getOperatorFunction <- function(symbol){
  switch(
    symbol,
    `+` = function(a, b) a + b,
    `-` = function(a, b) a - b,
    `*` = function(a, b) a * b,
    `/` = function(a, b) a / b)
}

toNumber <- function(str) {
  as.numeric(gsub(",", ".", str))
}


toChar <- function(num) {
  gsub("\\.", ",", as.character(num))
}
