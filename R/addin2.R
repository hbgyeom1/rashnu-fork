addin2 <- function() {
  app_dir <- system.file("shiny", package = "rashnu")
  shiny::runApp(app_dir)
}
