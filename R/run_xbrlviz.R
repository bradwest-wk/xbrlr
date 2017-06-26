#' Run Shiny App Visualization
#'
#' Executes the Shiny app for interactive xbrl taxonomy visualization
#'
#' @export
#' @return The shiny app
#'
runShinyApp <- function() {
    appDir <- system.file("shiny", "xbrlviz", package = "xbrlr")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `xbrlr`.",
             call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
