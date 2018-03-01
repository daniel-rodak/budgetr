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
      menuItem("Ustawienia budżetu", tabName = "settings", icon = icon("cog", lib = "glyphicon")),
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
        tabName = 'settings',
        fluidRow(
          box(width = 12, title = "Konta", collapsible = TRUE,
            box(
              width = 6, title = "Nowe konto",
              textInput('newAccName', 'Nazwa konta:'),
              numericInput('newAccInit', "Saldo początkowe:", value = 0),
              actionButton('addNewAcc', "Dodaj konto")
            ),
            box(
              width = 6, title = "Lista kont",
              tableOutput('accList')
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Kategorie", collapsible = TRUE,
            column(width = 6,
              box(
                width = NULL, title = "Nowa kategoria",
                textInput('newCatName', 'Nazwa kategorii:'),
                selectInput('newCatBudgCat', "Kategoria budżetowa:",
                            choices = budgetFile$getBudgetCategories()),
                actionButton('addNewCat', "Dodaj kategorię")
              ),
              box(
                width = NULL, title = "Nowa kategoria budżetowa",
                textInput('newBudgCatName', 'Nazwa kategorii budżetowej:'),
                actionButton('addNewBudgCat', "Dodaj kategorię budżetową")
              )
            ),
            column(width = 6,
              box(
                width = NULL, title = "Lista kategorii",
                tableOutput('catList')
              )
            )
          )
        )
      ),

      tabItem(
        tabName = 'import',
        fluidRow(
          box(
            width = 12, title = "Zaimportowane dane",
            rHandsontableOutput('dataTable'),
            textOutput('test1')
          )
        )
      )
    )
  )
)
