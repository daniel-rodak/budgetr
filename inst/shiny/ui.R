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
      menuItem("Transakcje", tabName = 'trans', icon = icon("list-alt", lib = "glyphicon")),
      menuItem("Importuj dane", tabName = "import", icon = icon("download"))
    ),
    conditionalPanel(condition = "input.menu1 == 'import'",
      fileInput("inputData", "Wybierz plik", accept = c("text/csv", "text/plain", "text/qif")),
      selectInput("fileType", "Rodzaj pliku",
                  choices = c("QIF" = "QIF", "mBank" = "mbank", "Idea Bank" = "idea")),
      actionButton("loadFile", "Importuj"),
      selectInput("addDataAcc", "Wybierz konto docelowe",
                  choices = budgetFile$getAccounts()),
      actionButton("addData", "Wgraj transakcje")
    ),
    conditionalPanel(condition = "input.menu1 == 'trans'",
      selectInput("transDataAcc", "Wybierz konto docelowe",
                  choices = budgetFile$getAccounts())
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
            column(width = 6,
              box(
                width = NULL, title = "Nowe konto",
                textInput('newAccName', 'Nazwa konta:'),
                numericInput('newAccInit', "Saldo początkowe:", value = 0),
                actionButton('addNewAcc', "Dodaj konto")
              ),
              box(
                width = NULL, title = "Usuń konto", collapsible = TRUE, collapsed = TRUE,
                selectInput('delAccName', 'Nazwa konta:',
                            choices = budgetFile$getAccounts()),
                actionButton('delAcc', "Usuń konto")
              )
            ),
            column(width = 6,
              box(
                width = 6, title = "Lista kont",
                tableOutput('accList')
              )
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
              ),
              box(
                width = NULL, title = "Usuń kategorię", collapsible = TRUE, collapsed = TRUE,
                selectInput('delCatName', 'Nazwa kategorii do usunięcia:',
                            choices = unname(budgetFile$getCategories())),
                uiOutput("mvCatNameUI"),
                actionButton('delCat', "Usuń kategorię")
              ),
              box(
                width = NULL, title = "Usuń kategorię budżetową", collapsible = TRUE, collapsed = TRUE,
                selectInput('delBudgCatName', 'Nazwa kategorii budżetowej:',
                            choices = budgetFile$getBudgetCategories()),
                actionButton('delBudgCat', "Usuń kategorię budżetową")
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
        tabName = 'trans',
        fluidRow(
          box(
            width = 12,
            title = "Transakcje",
            tableOutput("transData")
          )
        )
      ),

      tabItem(
        tabName = 'import',
        fluidRow(
          box(
            width = 12,
            title = "Zaimportowane dane",
            rHandsontableOutput('dataTable', height = "30vh")
          )
        ),
        fluidRow(
          box(
            width = 6, collapsible = TRUE,
            actionButton('splitTrans', "Podziel transakcję"),
            actionButton('applySplit', "Akceptuj"),
            tableOutput('selTransTable'),
            fluidRow(
              column(
                12,
                textOutput('leftAmount'),
                rHandsontableOutput('splitTable', height = "20vh")
              )
            )
          )
        )
      )
    )
  )
)
