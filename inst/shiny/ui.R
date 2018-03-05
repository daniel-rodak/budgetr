dashboardPage(
  dashboardHeader(
    title = "Budżet domowy",
    tags$li(
      class = "dropdown",
      actionButton("saveBut", "", icon = icon("floppy-disk", lib = 'glyphicon'),
                   style = "width: 50px; height: 50px; background-color: #3c8dbc; border: none; font-size: 1.5em;")
    ),
    tags$li(class = 'dropdown', textOutput('loadedBudget'))
  ),

  dashboardSidebar(
    sidebarMenu(id = 'menu1',
      menuItem("Otwórz/Zapisz", tabName = "os", icon = icon("floppy-disk", lib = "glyphicon")),
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
        tabName = 'os',
        fluidRow(
          box(
            width = 6, title = "Otwórz budżet",
            shinyFilesButton('openBdgt', "Wybierz plik", "Wybierz plik budżetu",
                            multiple = FALSE),
            textOutput('loadTest')
          ),
          box(
            width = 6, title = "Zapisz budżet",
            shinySaveButton('saveBdgt', "Wybierz lokalizację",
                            "Wybierz lokalizację do zapisu",
                            filetype = list(RDS = 'rds')),
            textOutput('saveTest')
          )
        )
      ),

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
            width = 12,
            title = "Zaimportowane dane",
            rHandsontableOutput('dataTable', height = "50vh")
          )
        ),
        fluidRow(
          box(
            width = 12,
            actionButton('splitTrans', "Podziel transakcję"),
            actionButton('applySplit', "Akceptuj"),
            rHandsontableOutput('selTransTable'),
            tags$br(),
            fluidRow(
              column(
                6,
                # sliderInput('numSplitCat', 'Liczba podkategorii',
                #             min = 2, max = max(2, length(budgetFile$getCategories())),
                #             step = 1, value = 2, width = "50%"),
                textOutput('leftAmount')
              ),
              column(
                6,
                rHandsontableOutput('splitTable')
              )
            )
          )
        )
      )
    )
  )
)
