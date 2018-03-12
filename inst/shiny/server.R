library(magrittr)

function(input, output, session) {
  timer <- reactiveTimer(100)
  budgetName <- reactive({
    timer()
    budgetFile$name
  })
  output$loadedBudget <- renderText(budgetName())

# Save and load budget ----------------------------------------------------


  # roots = c(home = '~/Desktop', wd = '.', base = '~/..')
  roots = c(home = '~/../Desktop')
  shinyFileChoose(input, 'openBdgt', roots = roots,
                  filetypes=c('', 'rds'), session = session)
  shinyFileSave(input, 'saveBdgt', roots = roots,
                filetypes=c('', 'rds'), session = session)

  observeEvent(input$openBdgt, {
    fi <- parseFilePaths(roots, input$openBdgt)
    if (nrow(fi) == 1) {
      budgetFile <<- budget$new(as.character(fi$datapath))
      updateSelectInput(session, "addDataAcc", choices = budgetFile$getAccounts())
      updateSelectInput(session, "transDataAcc", choices = budgetFile$getAccounts())
      updateSelectInput(session, 'newCatBudgCat', choices = budgetFile$getBudgetCategories())
      updateSelectInput(session, "delAccName", choices = budgetFile$getAccounts())
      updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()))
      updateSelectInput(session, "delBudgCat", choices = budgetFile$getBudgetCategories())
      budgetCats(unname(budgetFile$getCategories()))
    }
  })

  observeEvent(input$saveBdgt, {
    fi <- parseSavePath(roots, input$saveBdgt)
    if (nrow(fi) == 1) {
      budgetFile$save(as.character(fi$datapath))
    }
  })

  observeEvent(input$saveBut, {
    tr <- try(budgetFile$save())
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
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
    removeModal(session)
    showNotification(sprintf("Usunięto konto %s", input$delAccName), type = 'message')
  })

  output$accList <- renderTable({
    timer()
    data.frame(
      Konto = budgetFile$getAccounts(),
      Saldo = budgetFile$getAccountBalances()
    )
  })

  budgetCats <- reactiveVal(unname(budgetFile$getCategories()))

  observeEvent(input$addNewCat, {
    tr <- try(budgetFile$addCategory(input$newCatName, input$newCatBudgCat))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newCatName', value = "")
    budgetCats(unname(budgetFile$getCategories()))
    updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()))
  })

  output$mvCatNameUI <- renderUI({
    selectInput('mvCatName', 'Gdzie przenieść transakcje:',
                choices = setdiff(unname(budgetFile$getCategories()), input$delCatName))
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
    updateSelectInput(session, "delCatName", choices = unname(budgetFile$getCategories()))
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
    updateSelectInput(session, "delBudgCat", choices = budgetFile$getBudgetCategories())
  })

  output$catList <- renderTable({
    timer()
    cats <- budgetFile$getCategories()
    data.frame(
      Kategoria = cats,
      `Kategoria Budżetowa` = names(cats)
    )
  })

# Transaction data --------------------------------------------------------

  dfTrans <- reactiveVal()
  observeEvent(input$transDataAcc, {
    req(input$transDataAcc)
    dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
  })
  output$transData <- DT::renderDataTable(dfTrans(), filter = 'top',
                                          options = list(
                                            searching = FALSE
                                          ))

  dfTransEdit <- eventReactive(input$transData_rows_selected, {
    req(dfTrans())
    dfTrans()[input$transData_rows_selected, budgetr:::CNSTtransactionCols]
  })

  output$transEditTable <- renderRHandsontable({
    req(dfTransEdit())
    rhandsontable(dfTransEdit(),
                  stretchH = "all", selectCallback = TRUE) %>%
      hot_context_menu(allowColEdit = FALSE, allowRowEdit = FALSE) %>%
      hot_col(col = "Category", type = "autocomplete",
              source = budgetCats(),
              strict = TRUE)
  })

  observeEvent(input$delTrans, {
    req(dfTransEdit())
    req(input$transDataAcc)
    trIds <- row.names(dfTransEdit())
    tr <- try(budgetFile$deleteTransaction(input$transDataAcc, trIds))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    } else if (isTruthy(input$transDataAcc)){
      dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
    }
  })

  observeEvent(input$applyEdit, {
    req(dfTransEdit())
    req(input$transEditTable)
    req(input$transDataAcc)
    newTrans <- tr_to_r(input$transEditTable)
    trIds <- row.names(dfTransEdit())
    row.names(newTrans) <- trIds
    tr <- try({
      budgetFile$deleteTransaction(input$transDataAcc, trIds)
      budgetFile$addTransaction(input$transDataAcc, newTrans)
    })
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
    req(input$fileType)
    if (input$fileType == "QIF") {
      dfrm <- try(readQIF(input$inputData$datapath))
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
    DF(budgetr:::CNSTtrOneRowTemplate)
  })

  output$dataTable <- renderRHandsontable({
    req(DF())
    rhandsontable(DF(), stretchH = "all", selectCallback = TRUE) %>%
      hot_context_menu(allowColEdit = FALSE) %>%
      hot_col(col = "Category", type = "autocomplete",
              source = budgetCats(),
              strict = TRUE)
  })

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

  observeEvent(input$addData, {
    req(input$dataTable)
    trans <- try(tr_to_r(input$dataTable))
    if (inherits(trans, 'try-error')) {
      showNotification(trans, type = 'error', duration = 20)
    } else {
      tr <- try(budgetFile$addTransaction(input$addDataAcc, trans))
      if (inherits(tr, 'try-error')) {
        showNotification(tr, type = 'error', duration = 20)
      } else if (isTruthy(input$transDataAcc)){
        dfTrans(budgetFile$getTransactionTable(input$transDataAcc))
        DF(NULL)
        DF_sel(NULL)
        output$splitTable <- renderRHandsontable({NULL})
      }
    }
  })
}
