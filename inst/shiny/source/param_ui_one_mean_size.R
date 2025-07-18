param_ui_one_mean_size <- function() {
  tagList(
    selectInput("test_type", "Test Type", choices = c(
      "2-sided" = "2-side",
      "1-sided" = "1-side",
      "Non-inferiority" = "non-inferiority",
      "Equivalence" = "equivalence"
    )),
    radioButtons("mode", "Calculate", choices = c("Sample Size" = "size", "Power" = "power")),
    numericInput("mu", "mu", NULL),
    numericInput("mu0", "mu0", NULL),
    conditionalPanel(
      "input.test_type == 'non-inferiority' || input.test_type == 'equivalence'",
      numericInput("delta", "delta", NULL)
    ),
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
