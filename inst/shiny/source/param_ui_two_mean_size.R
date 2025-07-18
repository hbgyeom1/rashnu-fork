param_ui_two_mean_size <- function() {
  tagList(
    selectInput("test_type", "Test Type", choices = c(
      "2-sided" = "2-side",
      "1-sided" = "1-side",
      "Non-inferiority" = "non-inferiority",
      "Equivalence" = "equivalence"
    )),
    radioButtons("mode", "Calculate", choices = c("Sample Size" = "size", "Power" = "power")),
    numericInput("muA", "muA", NULL),
    numericInput("muB", "muB", NULL),
    conditionalPanel(
      "input.test_type == 'non-inferiority' || input.test_type == 'equivalence'",
      numericInput("delta", "delta", NULL)
    ),
    numericInput("kappa", "Kappa", 1),
    conditionalPanel(
      "input.test_type != '1-side'",
      numericInput("sd", "sd", NULL)
    ),
    conditionalPanel(
      "input.test_type == '1-side'",
      numericInput("sdA", "sdA", NULL),
      numericInput("sdB", "sdB", NULL)
    ),
    numericInput("alpha", "alpha", 0.05),
    conditionalPanel(
      "input.mode == 'size'",
      numericInput("beta", "beta", 0.2)
    ),
    conditionalPanel(
      "input.mode == 'power' && input.test_type == '1-side'",
      numericInput("nA", "nA", NULL)
    ),
    conditionalPanel(
      "input.mode == 'power' && input.test_type != '1-side'",
      numericInput("nB", "nB", NULL)
    )
  )
}
