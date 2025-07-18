param_ui_one_norm_size <- function() {
  tagList(
    radioButtons("mode", "Calculate", choices = c("Sample Size" = "size", "Power" = "power")),
    numericInput("mu", "mu", NULL),
    numericInput("mu0", "mu0", NULL),
    numericInput("sd", "sd", NULL),
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
