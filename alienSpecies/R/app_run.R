#' Run the shiny application with specific settings and install missing dependencies
#' @return no return value
#' @importFrom shiny runApp
#' @export
runShiny <- function() {
  
  # Run the application
  runApp(appDir = system.file("app", package = "alienSpecies"))
  
}




