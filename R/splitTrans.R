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
        calcColumn(12, class = 'calc-head', verbatimTextOutput(ns('state'), placeholder = TRUE))
      ),
      fluidRow(
        calcColumn(12, class = 'calc-head', textInput(ns('resIn'), label = NULL))
      ),
      fluidRow(
        calcColumn(3, calcActionButton(ns('AC'), 'AC')),
        calcColumn(3, calcActionButton(ns('pm'), '+/-')),
        calcColumn(3, calcActionButton(ns('percent'), '%')),
        calcColumn(3, calcActionButton(ns('divide'), '/'))
      ),
      fluidRow(
        calcColumn(3, calcActionButton(ns('d7'), '7')),
        calcColumn(3, calcActionButton(ns('d8'), '8')),
        calcColumn(3, calcActionButton(ns('d9'), '9')),
        calcColumn(3, calcActionButton(ns('times'), '*'))
      ),
      fluidRow(
        calcColumn(3, calcActionButton(ns('d4'), '4')),
        calcColumn(3, calcActionButton(ns('d5'), '5')),
        calcColumn(3, calcActionButton(ns('d6'), '6')),
        calcColumn(3, calcActionButton(ns('minus'), '-'))
      ),
      fluidRow(
        calcColumn(3, calcActionButton(ns('d1'), '1')),
        calcColumn(3, calcActionButton(ns('d2'), '2')),
        calcColumn(3, calcActionButton(ns('d3'), '3')),
        calcColumn(3, calcActionButton(ns('plus'), '+'))
      ),
      fluidRow(
        calcColumn(6, calcActionButton(ns('d0'), '0')),
        calcColumn(3, calcActionButton(ns('comma'), ',')),
        calcColumn(3, calcActionButton(ns('eq'), '='))
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
  firstVal <- reactiveVal(value = NA)
  secondVal <- reactiveVal(value = NA)
  operator <- reactiveVal(value = getOperatorFunction("+"))
  charOp <- reactiveVal(value = "+")

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
    if (!grepl(",", display())) {
      display(input$resIn)
      display(paste0(display(), ","))
    }
  })
  observeEvent(input$plus, {
    if (is.na(firstVal())) {
      display(input$resIn)
      firstVal(toNumber(display()))
      display("")
    } else {
      display(input$resIn)
      secondVal(toNumber(display()))
      if (!is.na(secondVal())) {
        result <- toChar(operator()(firstVal(), secondVal()))
        display("")
        firstVal(toNumber(result))
        secondVal(NA)
      }
    }
    operator(getOperatorFunction("+"))
    charOp("+")
  })
  observeEvent(input$minus, {
    if (is.na(firstVal())) {
      display(input$resIn)
      firstVal(toNumber(display()))
      display("")
    } else {
      display(input$resIn)
      secondVal(toNumber(display()))
      if (!is.na(secondVal())) {
        result <- toChar(operator()(firstVal(), secondVal()))
        display("")
        firstVal(toNumber(result))
        secondVal(NA)
      }
    }
    operator(getOperatorFunction("-"))
    charOp("-")
  })
  observeEvent(input$times, {
    if (is.na(firstVal())) {
      display(input$resIn)
      firstVal(toNumber(display()))
      display("")
    } else {
      display(input$resIn)
      secondVal(toNumber(display()))
      if (!is.na(secondVal())) {
        result <- toChar(operator()(firstVal(), secondVal()))
        display("")
        firstVal(toNumber(result))
        secondVal(NA)
      }
    }
    operator(getOperatorFunction("*"))
    charOp("*")
  })
  observeEvent(input$divide, {
    if (is.na(firstVal())) {
      display(input$resIn)
      firstVal(toNumber(display()))
      display("")
    } else {
      display(input$resIn)
      secondVal(toNumber(display()))
      if (!is.na(secondVal())) {
        result <- toChar(operator()(firstVal(), secondVal()))
        display("")
        firstVal(toNumber(result))
        secondVal(NA)
      }
    }
    operator(getOperatorFunction("/"))
    charOp("/")
  })
  observeEvent(input$eq, {
    if (!is.na(firstVal())) {
      display(input$resIn)
      secondVal(toNumber(display()))
      if (!is.na(secondVal())) {
        display(input$resIn)
        result <- toChar(operator()(firstVal(), secondVal()))
        display("")
        firstVal(toNumber(result))
        secondVal(NA)
      }
    }
  })
  observeEvent(input$AC, {
    display("")
    firstVal(NA)
    secondVal(NA)
    operator(getOperatorFunction("+"))
    charOp("+")
  })
  observeEvent(input$pm, {
    req(display())
    display(toChar(-toNumber(display())))
  })
  observeEvent(input$percent, {
    req(display())
    display(toChar(toNumber(display()) / 100.0))
  })

  output$state <- renderText({
    if (!is.na(firstVal())) {
      paste(firstVal(), charOp())
    }
  })
  observeEvent(display(), {
    updateTextInput(session, "resIn", value = display())
  })

  observeEvent(input$resIn, {
    x <- input$resIn
    lastChar <- substr(x, nchar(x), nchar(x))
    validChars <- c(0:9, ",", ".")
    if (!(lastChar %in% validChars)) {
      updateTextInput(session, "resIn", value = gsub(".$", "", x))
    }
  })

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
  if (str == "")
    ret <- NA
  else
    ret <- as.numeric(gsub(",", ".", str))
  return(ret)
}


toChar <- function(num) {
  gsub("\\.", ",", as.character(num))
}

calcActionButton <- function(inputId, label, icon = NULL, width = NULL, ...) {
  btn <- shiny::actionButton(inputId, label, icon = NULL, width = NULL, ...)
  btn$attribs$class <- paste(btn$attribs$class, "calc-btn")
  btn
}

calcColumn <- function(width, ..., offset = 0, class = 'calc-col') {
  clm <- column(width, ..., offset = 0)
  clm$attribs$class <- paste(clm$attribs$class, class)
  clm
}
