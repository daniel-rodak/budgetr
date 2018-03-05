library(budgetr)
acc <- "Test Account"
trn <- readQIF("tests/testdata/money.qif")
init <- trn$Amount[1]
trn <- trn[2:nrow(trn), ]
cats <- unique(trn$Category)
save.path <- "tests/testdata/budget.rds"

myBudget <- budget$new()
bCat <- myBudget$getBudgetCategories()
myBudget$addAccount(acc, init)
myBudget$addCategory(cats, bCat[c(12, 4, 6, 12)])
myBudget$addTransaction(acc, trn)
myBudget$addAccount('Idea', 100)
myBudget$addAccount('mBank', 40)
myBudget$addAccount('Pekao', 47)
myBudget$addAccount('Getin', 189)
myBudget$getAccounts()
myBudget$getAccountBalances()
myBudget$getAccountInitialBalances()
myBudget$getTransactionTable('Getin')
myBudget$save(save.path)
bdgt <- readRDS(save.path)

myNewBudget <- budget$new(save.path)
myNewBudget$getAccounts()
myNewBudget$getCategories()
myNewBudget$getTransactionTable(acc)
