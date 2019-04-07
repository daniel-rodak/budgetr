library(magrittr)
library(plotly)

function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  timer <- reactiveTimer(100)
  budgetName <- reactive({
    timer()
    budgetFile$name
    print(budgetFile$name)
  })
  output$loadedBudget <- renderText(budgetName())

# Save and load budget ----------------------------------------------------

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$saveFile
    },
    handlerExpr = {
      if (input$saveFile > 0) {
        path <- choose.filename(default = readFilenameSelectInput(session, 'saveFile'),
                                caption = sprintf("Zapisz budżet %s", budgetName()))

        if (!is.na(path)) {
          updateFilenameSelectInput(session, 'saveFile', value = path)

          tr <- try(budgetFile$save(path))
          if (inherits(tr, 'try-error')) {
            showNotification(tr, type = 'error', duration = 20)
          }
        }
      }
    }
  )

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$loadBdgtFile
    },
    handlerExpr = {
      if (input$loadBdgtFile > 0) {
        path <- choose.file(default = readFileSelectInput(session, 'loadBdgtFile'),
                            caption = "Otwórz budżet")
        print(path)
        if (!is.na(path)) {
          updateFileSelectInput(session, 'loadBdgtFile', value = path)

          tr <- try(budgetFile <<- budget$new(path))
          if (inherits(tr, 'try-error')) {
            showNotification(tr, type = 'error', duration = 20)
          } else {
            updateSelectInput(session, "addDataAcc", choices = budgetFile$getAccounts())
            updateSelectInput(session, "transDataAcc", choices = budgetFile$getAccounts())
            updateSelectInput(session, 'newCatBudgCat', choices = budgetFile$getBudgetCategories())
            updateSelectInput(session, "editAccName", choices = budgetFile$getAccounts())
            updateSelectInput(session, "delAccName", choices = budgetFile$getAccounts())
            updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()[names(budgetFile$getCategories()) != "Systemowe"]))
            updateSelectInput(session, "delBudgCatName", choices = budgetFile$getBudgetCategories())
            updateSelectInput(session, "reportChoice",
                              choices = ifNull(rownames(budgetFile$listReports()), ""))
            budgetCats(unname(budgetFile$getCategories()))
          }
        }
      }
    }
  )

  observeEvent(input$saveBut, {
    budgetFile$save()
  })

# Budget settings ---------------------------------------------------------


  observeEvent(input$addNewAcc, {
    tr <- try(budgetFile$addAccount(input$newAccName, input$newAccInit))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newAccName', value = "")
    updateNumericInput(session, 'newAccInit', value = 0)
    updateSelectInput(session, "addDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "transDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "delAccName", choices = budgetFile$getAccounts())
    updateSelectInput(session, "editAccName", choices = budgetFile$getAccounts())
  })

  observeEvent(input$editAccName, {
    req(input$editAccName)
    updateNumericInput(
      session, 'newEditAccInit',
      value = budgetFile$getAccountInitialBalances()[[input$editAccName]]
    )
    updateTextInput(session, 'newEditAccName', value = input$editAccName)
  })

  observeEvent(input$editAcc, {
    # Edit initial balance
    if (input$newEditAccInit != budgetFile$getAccountInitialBalances()[[input$editAccName]]) {
      tr <- try(budgetFile$setAccountInitialBalance(input$editAccName, input$newEditAccInit))
      if (inherits(tr, 'try-error')) {
        showNotification(tr, type = 'error', duration = 20)
      }
    }
    # Rename account
    if (input$newEditAccName != input$editAccName) {
      tr <- try(budgetFile$renameAccount(input$editAccName, input$newEditAccName))
      if (inherits(tr, 'try-error')) {
        showNotification(tr, type = 'error', duration = 20)
      }
    }
    updateSelectInput(session, "addDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "transDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "delAccName", choices = budgetFile$getAccounts())
    updateSelectInput(session, "editAccName", choices = budgetFile$getAccounts(), selected = input$newEditAccName)
    updateTextInput(session, 'newEditAccName', value = input$newEditAccName)
    updateNumericInput(session, 'newEditAccInit', value = budgetFile$getAccountInitialBalances()[[input$newEditAccName]])
  })

  observeEvent(input$delAcc, {
    showModal(modalDialog(
      title = "Usuwanie konta",
      "Usunięcie konta usunie wszystkie transakcje powiązane z tym kontem. Kontunuować?",
      footer = fluidRow(
        actionButton('delAccConfirm', 'Kontynuuj'),
        modalButton('Anuluj')
      )
    ))
  })

  observeEvent(input$delAccConfirm, {
    req(input$delAccName)
    budgetFile$deleteAccount(input$delAccName)
    updateSelectInput(session, "addDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "transDataAcc", choices = budgetFile$getAccounts())
    updateSelectInput(session, "delAccName", choices = budgetFile$getAccounts())
    updateSelectInput(session, "editAccName", choices = budgetFile$getAccounts())
    removeModal(session)
    showNotification(sprintf("Usunięto konto %s", input$delAccName), type = 'message')
  })

  output$accList <- renderTable({
    timer()
    dfr <- data.frame(
      Konto = budgetFile$getAccounts(),
      Saldo = budgetFile$getAccountBalances()
    )
    rbind(dfr, data.frame(Konto = "Total", Saldo = sum(dfr$Saldo)))
  })

  budgetCats <- reactiveVal(unname(budgetFile$getCategories()))

  observeEvent(input$addNewCat, {
    tr <- try(budgetFile$addCategory(input$newCatName, input$newCatBudgCat))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newCatName', value = "")
    budgetCats(unname(budgetFile$getCategories()))
    updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()[names(budgetFile$getCategories()) != "Systemowe"]))
  })

  output$mvCatNameUI <- renderUI({
    selectInput('mvCatName', 'Gdzie przenieść transakcje:',
                choices = setdiff(unname(budgetFile$getCategories()[names(budgetFile$getCategories()) != "Systemowe"]), input$delCatName))
  })

  observeEvent(input$delCat, {
    showModal(modalDialog(
      title = "Usuwanie kategorii",
      "Tej operacji nie da się cofnąć. Możesz tylko ponownie dodać kategorię i ręcznie zmienić kategorie wybranych transakcji. Kontunuować?",
      footer = fluidRow(
        actionButton('delCatConfirm', 'Kontynuuj'),
        modalButton('Anuluj')
      )
    ))
  })

  observeEvent(input$delCatConfirm, {
    req(input$delCatName)
    req(input$mvCatName)
    budgetFile$moveCategory(input$delCatName, input$mvCatName)
    budgetFile$deleteCategory(input$delCatName)
    budgetCats(unname(budgetFile$getCategories()))
    updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()[names(budgetFile$getCategories()) != "Systemowe"]))
    removeModal(session)
    showNotification(sprintf("Usunięto kategorię %s", input$delCatName), type = 'message')
  })

  observeEvent(input$addNewBudgCat, {
    tr <- try(budgetFile$addBudgetCategory(input$newBudgCatName))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newBudgCatName', value = "")
    updateSelectInput(session, 'newCatBudgCat', choices = budgetFile$getBudgetCategories())
    updateSelectInput(session, "delBudgCatName", choices = budgetFile$getBudgetCategories())
  })

  observeEvent(input$delBudgCat, {
    showModal(modalDialog(
      title = "Usuwanie kategorii budżetowej",
      "Upewnij się, że nie ma żadnych kategorii należących do wybranej kategorii budżetowej. Inaczej nie da się usunąć kategorii budżetowej. Kontunuować?",
      footer = fluidRow(
        actionButton('delBudgCatConfirm', 'Kontynuuj'),
        modalButton('Anuluj')
      )
    ))
  })

  observeEvent(input$delBudgCatConfirm, {
    req(input$delBudgCatName)
    tr <- try(budgetFile$deleteBudgetCategory(input$delBudgCatName))
    if (inherits(tr, 'try-error')){
      removeModal(session)
      showNotification(tr, type = 'error', duration = 20)
    } else{
      budgetCats(unname(budgetFile$getCategories()))
      updateSelectInput(session, 'newCatBudgCat', choices = budgetFile$getBudgetCategories())
      updateSelectInput(session, "delBudgCatName", choices = budgetFile$getBudgetCategories())
      removeModal(session)
      showNotification(sprintf("Usunięto kategorię budżetową %s", input$delBudgCatName), type = 'message')
    }
  })

  output$catList <- renderTable({
    timer()
    cats <- budgetFile$getCategories()
    ret <- data.frame(
      v1 = cats,
      v2 = names(cats)
    )
    ret$v3 <- c("W" = 1, "D" = 2, "S" = 3)[substring(ret$v2, 1, 1)]
    ret <- ret[order(ret$v3, ret$v1), ]
    colnames(ret) <- enc2utf8(c("Kategoria", "Kategoria budżetowa"))
    ret[, 1:2]
  })

# Transaction data --------------------------------------------------------

  dfTrans <- reactiveVal()
  observeEvent(input$transDataAcc, {
    req(input$transDataAcc)
    dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
  })
  output$transData <- DT::renderDataTable(dfTrans(), filter = 'top',
                                          options = list(
                                            searching = FALSE,
                                            language = CNSTDTPLLanguage,
                                            info = FALSE,
                                            paging = FALSE,
                                            scrollY = '50vh',
                                            scrollCollapse= TRUE
                                          ), colnames = CNSTtransactionColsPL)

  dfTransEdit <- reactiveVal()
  observeEvent(input$transData_rows_selected, {
    req(dfTrans())
    dfTransEdit(dfTrans()[input$transData_rows_selected, CNSTtransactionCols])
  })

  output$transEditTable <- renderRHandsontable({
    req(dfTransEdit())
    rhandsontable(dfTransEdit(), stretchH = "all", selectCallback = TRUE,
                  colHeaders = CNSTtransactionColsPL[1:6]) %>%
      hot_context_menu(allowColEdit = FALSE, allowRowEdit = FALSE) %>%
      hot_col(col = "Kategoria", type = "autocomplete",
              source = budgetCats(),
              strict = TRUE)
  })

  exTrigger <- reactiveVal(0)
  exNewData <- callModule(splitTransaction, "exTransSplit",
                        reactive(input$transEditTable),
                        reactive(input$transEditTable_select),
                        budgetCats,
                        exTrigger)

  observeEvent(input$exApplySplit, {
    req(exNewData())
    dfTransEdit(exNewData())
  })

  observeEvent(input$applyEdit, {
    req(dfTransEdit())
    req(input$transEditTable)
    req(input$transDataAcc)
    newTrans <- tr_to_r(input$transEditTable)
    trIds <- row.names(dfTrans()[input$transData_rows_selected, ])
    if (length(trIds) == nrow(newTrans))
      row.names(newTrans) <- trIds
    tr <- try({
      budgetFile$deleteTransaction(input$transDataAcc, trIds, input$autoSys2)
      budgetFile$addTransaction(input$transDataAcc, newTrans, input$autoSys2)
    })
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    } else if (isTruthy(input$transDataAcc)){
      dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
      exTrigger(exTrigger() + 1)
    }
  })

  observeEvent(input$delTrans, {
    req(dfTransEdit())
    req(input$transDataAcc)
    trIds <- row.names(dfTransEdit())
    tr <- try(budgetFile$deleteTransaction(input$transDataAcc, trIds, input$autoSys2))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    } else if (isTruthy(input$transDataAcc)){
      dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
    }
  })

# Import data -------------------------------------------------------------

  DF <- reactiveVal()

  observeEvent(input$loadFile, {
    req(input$inputData)
    str(input$inputData)
    req(input$fileType)
    if (input$fileType == "QIF") {
      dfrm <- try(readQIF(input$inputData$datapath, input$aggSplits))
    } else {
      dfrm <- try(readBank(input$inputData$datapath, input$fileType))
    }
    if (inherits(dfrm, 'try-error')) {
      showNotification(dfrm, type = 'error', duration = 20)
    } else {
      DF(dfrm)
    }
  })

  observeEvent(input$manualTrans, {
    DF(CNSTtrOneRowTemplate)
  })

  output$dataTable <- renderRHandsontable({
    req(DF())
    rhandsontable(DF(), stretchH = "all", selectCallback = TRUE,
                  colHeaders = CNSTtransactionColsPL[1:6]) %>%
      hot_context_menu(allowColEdit = FALSE) %>%
      hot_col(col = "Kategoria", type = "autocomplete",
              source = budgetCats(),
              strict = TRUE)
  })

  trigger <- reactiveVal(0)
  newData <- callModule(splitTransaction, "newTransSplit",
                        reactive(input$dataTable),
                        reactive(input$dataTable_select),
                        budgetCats,
                        trigger)

  observeEvent(input$applySplit, {
    req(newData())
    DF(newData())
  })

  observeEvent(input$addData, {
    req(input$dataTable)
    trans <- try(tr_to_r(input$dataTable))
    if (inherits(trans, 'try-error')) {
      showNotification(trans, type = 'error', duration = 20)
    } else {
      tr <- try(budgetFile$addTransaction(input$addDataAcc, trans, input$autoSys))
      if (inherits(tr, 'try-error')) {
        showNotification(tr, type = 'error', duration = 20)
      } else if (isTruthy(input$transDataAcc)){
        dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
        DF(NULL)
        trigger(trigger() + 1)
      }
    }
  })

# Reports -----------------------------------------------------------------

  ### Show report
  repShow <- reactiveVal()
  observeEvent(input$reportChoice, {
    req(input$reportChoice)
    repShow(budgetFile$getReport(input$reportChoice)$show(objOnly = TRUE))
  })
  output$reportVis <- renderUI({
    if (inherits(repShow(), "data.frame")) {
      DT::DTOutput("reportTable")
    } else if (inherits(repShow(), "plotly")) {
     plotly::plotlyOutput("reportChart")
    } else {
      NULL
    }
  })

  output$reportChart <- plotly::renderPlotly({
    if (inherits(repShow(), "plotly")) {
      repShow()
    } else {
      NULL
    }
  })

  output$reportTable <- DT::renderDT({
    if (inherits(repShow(), "data.frame")) {
      repShow()
    } else {
      NULL
    }
  }, filter = "none", selection = "none",
  options = list(
    searching = FALSE,
    language = CNSTDTPLLanguage,
    paging = FALSE
  ))

  ### Delete report
  observeEvent(input$deleteReport, {
    showModal(modalDialog(
      title = "Usuwanie raportu",
      sprintf("Usuwanie raportu '%s'. Kontunuować?", input$reportChoice),
      footer = fluidRow(
        actionButton('delRepConfirm', 'Kontynuuj'),
        modalButton('Anuluj')
      )
    ))
  })
  observeEvent(input$delRepConfirm, {
    budgetFile$deleteReport(input$reportChoice)
    updateSelectInput(session, "reportChoice", choices = rownames(budgetFile$listReports()))
    removeModal(session)
  })

  ### Edit report
  observeEvent(input$editReport, {
    showModal(reportSettings(FALSE, budgetFile,
                             budgetFile$getReport(input$reportChoice)$metaFiller()))
  })
  observeEvent(input$editReportConfirm, {
    req(input$reportChoice)
    setField <- function(field, value) {
      budgetFile$setReportField(input$reportChoice, field, value)
    }
    setField('type', input$repType)
    setField('rows', input$repRows)
    setField('cols', input$repCols)
    setField('accounts', input$repAccounts)
    setField('categories', input$repCategories)
    setField('noSys', input$repNoSys)
    if (input$repCustomDate) {
      setField('dateRange', input$repCustomDateRange)
    } else {
      setField('dateRange', input$repDateRange)
    }
    if (input$repName != input$reportChoice)
      setField('name', input$repName)
    repShow(budgetFile$getReport(input$repName)$show(objOnly = TRUE))
    updateSelectInput(session, "reportChoice",
                      choices = rownames(budgetFile$listReports()),
                      selected = input$repName)
    removeModal(session)
  })

  ### Add report
  observeEvent(input$addReport, {
    showModal(reportSettings(TRUE, budgetFile))
  })
  observeEvent(input$addReportConfirm, {
    if (input$repCustomDate) {
      dRange <- input$repCustomDateRange
    } else {
      dRange <- input$repDateRange
    }
    rep <- report$new(
      budgetFile,
      input$repName, input$repType, input$repRows, input$repCols,
      input$repAccounts, input$repCategories, dRange, input$repNoSys
    )
    budgetFile$addReport(rep)
    updateSelectInput(session, "reportChoice",
                      choices = rownames(budgetFile$listReports()),
                      selected = input$repName)
    repShow(budgetFile$getReport(input$repName)$show(objOnly = TRUE))
    removeModal(session)
  })

  ### Button reactivity handlers
  observe({
    if ('Wszystko' %in% input$repCategories) {
      updateSelectInput(session, 'repCategories', selected = budgetCats())
    }
  })
  observeEvent(input$repNoSys, {
    if (input$repNoSys) {
      selCache <- input$repCategories
      chcs <- c(list(Wszystko = "Wszystko"),
                namedVecToList(budgetFile$getCategories()))
      chcs$Systemowe <- NULL
      cats <- budgetFile$getCategories()
      cats <- cats[names(cats) == 'Systemowe']
      selCache <- setdiff(selCache, cats)
      updateSelectInput(session, 'repCategories',
                        choices = chcs,
                        selected = selCache)
    } else {
      selCache <- input$repCategories
      chcs <- c(list(Wszystko = "Wszystko"),
                namedVecToList(budgetFile$getCategories()))
      updateSelectInput(session, 'repCategories',
                        choices = chcs,
                        selected = selCache)
    }
  })
  observe({
    shinyjs::toggleState(id = "editReport", condition = input$reportChoice != "")
  })
}
