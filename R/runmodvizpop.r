#' @export
runmodvizpop <- function() {
  appDir <- system.file("shiny-apps", "myapp", package = "modvizpop")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `modvizpop`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
