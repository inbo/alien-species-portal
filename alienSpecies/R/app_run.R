#' Run the shiny application with specific settings and install missing dependencies
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @importFrom stats update
#' @importFrom remotes package_deps
#' @export
runShiny <- function(installDependencies = FALSE, ...) {
  
  # (1) Install all suggested R packages (see DESCRIPTION)
  if (installDependencies) {
    
    ## (a) CRAN packages
    update(package_deps(packages = system.file("app", package = "alienSpecies"), 
        dependencies = "Suggests"))
    
    
    ## (b) non-CRAN packages - by hand
#    if (!requireNamespace("rhandsontable")) {
#      
#      install_github("jrowen/rhandsontable")
#      
#    }
    
  }
  
  
  # (2) Run the application
  ldots <- list(...)
  
  if (is.null(ldots$appDir))
    ldots$appDir <- system.file("app", package = "alienSpecies")
  if (is.null(ldots$port))
    ldots$port <- 3838
  
  do.call(runApp, ldots)
  
}




