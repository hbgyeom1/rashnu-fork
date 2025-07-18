main_ui_sccs_size <- function() {
  withMathJax(
    tagList(
      h4("Result"),
      textOutput("result"),
      hr(),
      h4("Add later"),
      p("Add later")
    )
  )
}
