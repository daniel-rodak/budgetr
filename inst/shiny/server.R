library(magrittr)

function(input, output, session) {
  timer <- reactiveTimer(100)
  budgetName <- reactive({
    timer()
    budgetFile$name
  })
  output$loadedBudget <- renderText(budgetName())

  roots = c(wd = '~/../source/repos/budgetr/tests/testdata', home = '~', base = '~/..')
  shinyFileChoose(input, 'openBdgt', roots = roots,
                  filetypes=c('', 'rds'), session = session)
  shinyFileSave(input, 'saveBdgt', roots = roots,
                filetypes=c('', 'rds'), session = session)
  output$loadTest <- renderText(capture.output(str(parseFilePaths(roots, input$openBdgt))))
  output$saveTest <- renderText(capture.output(str(parseSavePath(roots, input$saveBdgt))))

  observeEvent(input$openBdgt, {
    fi <- parseFilePaths(roots, input$openBdgt)
    if (nrow(fi) == 1) {
      budgetFile <- budget$new(as.character(fi$datapath))
    }
  })

  observeEvent(input$saveBdgt, {
    fi <- parseSavePath(roots, input$saveBdgt)
    if (nrow(fi) == 1) {
      budgetFile$save(as.character(fi$datapath))
    }
  })

  observeEvent(input$addNewAcc, {
    tr <- try(budgetFile$addAccount(input$newAccName, input$newAccInit))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newAccName', value = "")
    updateNumericInput(session, 'newAccInit', value = 0)
  })

  output$accList <- renderTable({
    timer()
    data.frame(
      Konto = budgetFile$getAccounts(),
      Saldo = budgetFile$getAccountBalances()
    )
  })

  observeEvent(input$addNewCat, {
    tr <- try(budgetFile$addCategory(input$newCatName, input$newCatBudgCat))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newCatName', value = "")
  })

  observeEvent(input$addNewBudgCat, {
    tr <- try(budgetFile$addBudgetCategory(input$newBudgCatName))
    if (inherits(tr, 'try-error')) {
      showNotification(tr, type = 'error', duration = 20)
    }
    updateTextInput(session, 'newBudgCatName', value = "")
    updateSelectInput(session, 'newCatBudgCat', choices = budgetFile$getBudgetCategories())
  })

  output$catList <- renderTable({
    timer()
    cats <- budgetFile$getCategories()
    data.frame(
      Kategoria = cats,
      `Kategoria Budżetowa` = names(cats)
    )
  })

  DF <- eventReactive(input$loadFile, {
    req(input$inputData)
    req(input$fileType)
    if (input$fileType == "QIF") {
      dfrm <- readQIF(input$inputData$datapath)
    } else {
      dfrm <- readBank(input$inputData$datapath, input$fileType)
    }
    return(dfrm)
  })

  output$dataTable <- renderRHandsontable({
    req(DF())
    rhandsontable(DF(), stretchH = "all", selectCallback = TRUE) %>%
      hot_context_menu(allowColEdit = FALSE) %>%
      hot_col(col = "Category", type = "autocomplete",
              source = budgetFile$getCategories(), strict = TRUE)
  })

  observeEvent(input$splitTrans, {
    req(input$dataTable_select)
    sel <- input$dataTable_select$select
    if (sel$r != sel$r2) {
      showNotification("Wybierz jedną transakcję", type = 'warning', duration = 20)
    } else {
      if (is.null(input$dataTable)) {
        DF_sel <- DF()[sel$r, , drop = FALSE]
      } else {
        DF_sel <- hot_to_r(input$dataTable)[sel$r, , drop = FALSE]
      }

      output$selTransTable <- renderRHandsontable({
        rhandsontable(DF_sel, stretchH = "all") %>%
          hot_context_menu(allowColEdit = FALSE) %>%
          hot_col(col = "Category", type = "autocomplete",
                  source = budgetFile$getCategories(), strict = TRUE)
      })
    }
  })

  output$splitTable <- renderRHandsontable({
    req(input$selTransTable)
    DF_sel <- hot_to_r(input$selTransTable)
    nrep <- 1 # input$numSplitCat - 1
    dfrm <- data.frame(
      Kategoria = c(DF_sel$Category, rep("", nrep)),
      Kwota = c(DF_sel$Amount, rep(0, nrep)),
      stringsAsFactors = FALSE
    )
    rhandsontable(dfrm, stretchH = "all") %>%
      hot_context_menu(allowColEdit = FALSE, allowRowEdit = FALSE) %>%
      hot_col(col = "Kategoria", type = "autocomplete",
              source = budgetFile$getCategories(), strict = TRUE)
  })

  output$leftAmount <- renderText({
    req(input$splitTable)
    req(input$selTransTable)
    DF_sel <- hot_to_r(input$selTransTable)
    dfSplit <- hot_to_r(input$splitTable)
    unassigned <- DF_sel$Amount - sum(dfSplit$Kwota)
    sprintf("Nieprzydzielona kwota: %0.2f zł", unassigned)
  })

  observeEvent(input$applySplit, {
    req(input$splitTable)
    req(input$selTransTable)
    DF_sel <- hot_to_r(input$selTransTable)
    dfSplit <- hot_to_r(input$splitTable)
    if (sum(dfSplit$Kwota) != DF_sel$Amount) {
      showNotification(paste("Suma podkategorii nie równa się", DF_sel$Amount),
                       type = 'warning', duration = 20)
    } else {
      str(dfSplit)
    }
  })
}
