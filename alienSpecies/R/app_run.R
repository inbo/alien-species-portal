#' Run the shiny application with specific settings and install missing dependencies
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @export
runShiny <- function(...) {
  
  # (1) Check S3 data - On UAT only, not PRD
  if (config::get("datacheck", file = system.file("config.yml", package = "alienSpecies")))
    errorApp <- tryCatch(
      testS3(),
      error = function(err)
        shinyApp(ui = fluidPage(
            tags$h3("Error during Data Check"),
            HTML(err$message)
          ), server = function(input, output, session){})
    )
  
  # (2) Run the application
  if (exists("errorApp") && is(errorApp, "shiny.appobj"))
    errorApp else 
    runApp(appDir = system.file("app", package = "alienSpecies"), ...)
  
  
}




