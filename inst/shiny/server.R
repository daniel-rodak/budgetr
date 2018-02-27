function(input, output, session) {
  budgetFile <- reactive(budget$new())

  output$loadedBudget <- renderMenu({
    notificationItem(text = budgetFile()$name, icon = icon("euro", lib = "glyphicon"))
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
    rhandsontable(DF(), stretchH = "all")
  })
}
