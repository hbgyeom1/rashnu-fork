main_ui_one_norm_size <- function() {
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
