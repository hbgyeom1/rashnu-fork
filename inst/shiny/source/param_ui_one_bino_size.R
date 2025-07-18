param_ui_one_bino_size <- function() {
  tagList(
    radioButtons("mode", "Calculate", choices = c("Sample Size" = "size", "Power" = "power")),
    numericInput("p", "p", NULL),
    numericInput("p0", "p0", NULL),
    numericInput("alpha", "alpha", 0.05),
    conditionalPanel(
      "input.mode == 'size'",
      numericInput("beta", "beta", 0.2)
    ),
    conditionalPanel(
      "input.mode == 'power'",
      numericInput("n", "n", NULL)
    )
  )
}
