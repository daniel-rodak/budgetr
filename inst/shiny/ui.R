dashboardPage(
  dashboardHeader(
    title = "Budżet domowy",
    dropdownMenuOutput("loadedBudget"),
    tags$li(
      class = "dropdown",
      actionButton("saveBut", "", icon = icon("floppy-disk", lib = 'glyphicon'),
                   style = "width: 50px; height: 50px; background-color: #3c8dbc; border: none; font-size: 1.5em;")
    )
  ),

  dashboardSidebar(
    sidebarMenu(id = 'menu1',
      menuItem("Importuj dane", tabName = "import", icon = icon("download"))
    ),
    conditionalPanel(condition = "input.menu1 == 'import'",
      fileInput("inputData", "Wybierz plik", accept = c("text/csv", "text/plain")),
      selectInput("fileType", "Rodzaj pliku",
                  choices = c("QIF" = "QIF", "mBank" = "mbank", "Idea Bank" = "idea")),
      actionButton("loadFile", "Importuj")
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'import',
        fluidRow(
          box(
            width = 12, title = "Zaimportowane dane",
            rHandsontableOutput('dataTable')
          )
        )
      )
    )
  )
)
