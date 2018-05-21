#' Function launching bidget app
#'
#' @author Daniel Rodak
#' @importFrom shiny runApp
#' @export
launch <- function() {
  shiny::runApp(system.file('shiny', package = 'budgetr'),
                launch.browser = TRUE)
}
