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
      menuItem("Dodaj transakcje", tabName = "import", icon = icon("save", lib = "glyphicon")),
      menuItem("Raport", tabName = "report", icon = icon("stats", lib = "glyphicon"))
    ),
    conditionalPanel(condition = "input.menu1 == 'import'",
      tags$h4("Importuj"),
      fileInput("inputData", "Wybierz plik", accept = c("text/csv", "text/plain", "text/qif")),
      selectInput("fileType", "Rodzaj pliku",
                  choices = c("QIF" = "QIF", "mBank" = "mbank", "Idea Bank" = "idea")),
      conditionalPanel(
        condition = "input.fileType == 'QIF'",
        checkboxInput("aggSplits", "Zagreguj transakcje")
      ),
      actionButton("loadFile", "Importuj"),
      tags$h4("Dodaj ręcznie"),
      actionButton("manualTrans", "Dodaj"),
      tags$h4("Wgraj do konta"),
      selectInput("addDataAcc", "Wybierz konto docelowe",
                  choices = budgetFile$getAccounts()),
      checkboxInput("autoSys", "Auto transakcje systemowe", value = TRUE),
      actionButton("addData", "Wgraj")
    ),
    conditionalPanel(condition = "input.menu1 == 'trans'",
      selectInput("transDataAcc", "Wybierz konto docelowe",
                  choices = budgetFile$getAccounts())
    ),
    conditionalPanel(condition = "input.menu1 == 'report'",
      selectInput("reportChoice", "Wybierz raport",
                  choices = rownames(budgetFile$listReports())),
      actionButton("editReport", "Modyfikuj"),
      actionButton("deleteReport", "Usuń"),
      actionButton("addReport", "Dodaj nowy")
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(
        rel = 'stylesheet',
        type = 'text/css',
        href = 'myStyle.css'
      )
    ),
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(
        tabName = 'os',
        fluidRow(
          box(
            width = 6, title = "Otwórz budżet",
            fileSelectInput('loadFile', label = '')
          ),
          box(
            width = 6, title = "Zapisz budżet",
            filenameSelectInput('saveFile', label = '')
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
                width = NULL, title = "Edytuj konto", collapsible = TRUE, collapsed = TRUE,
                selectInput('editAccName', 'Konta:',
                            choices = budgetFile$getAccounts()),
                textInput('newEditAccName', 'Nowa nazwa:'),
                numericInput('newEditAccInit', "Nowe saldo poczatkowe:", value = 0),
                actionButton('editAcc', "Edytuj konto")
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
                width = NULL, title = "Lista kont",
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
                            choices = unname(budgetFile$getCategories()[names(budgetFile$getCategories()) != "Systemowe"])),
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
            DT::dataTableOutput("transData")
          )
        ),
        fluidRow(
          box(
            width = 12,  collapsible = TRUE, collapsed = FALSE,
            title = "Edycja",
            actionButton('applyEdit', "Akceptuj"),
            actionButton('delTrans', "Usuń"),
            checkboxInput("autoSys2", "Auto transakcje systemowe", value = TRUE),
            rHandsontableOutput('transEditTable', height = '20vh')
          )
        ),
        fluidRow(
          column(
            12,
            splitTransactionUI("exTransSplit", actionButton('exApplySplit', "Akceptuj"))
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
          column(
            12,
            splitTransactionUI("newTransSplit", actionButton('applySplit', "Akceptuj"))
          )
        )
      ),

      tabItem(
        tabName = 'report',
        fluidRow(
          box(
            width = 12,
            uiOutput('reportVis')
          )
        )
      )
    )
  )
)
