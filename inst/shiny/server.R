library(magrittr)

function(input, output, session) {
  output$loadedBudget <- renderMenu({
    notificationItem(text = budgetFile$name, icon = icon("euro", lib = "glyphicon"))
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
    reactiveTimer(100)()
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
    reactiveTimer(100)()
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

  # output$test1 <- renderText(str(input$dataTable_select))
}
