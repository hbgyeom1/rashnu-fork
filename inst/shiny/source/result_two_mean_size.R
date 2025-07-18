result_two_mean_size <- function(input) {
  if (input$mode == "size") {
    two_mean_size(
      muA = input$muA,
      muB = input$muB,
      delta = input$delta,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else if (input$test_type == "1-side") {
    two_mean_size(
      muA = input$muA,
      muB = input$muB,
      delta = input$delta,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      alpha = input$alpha,
      nA = input$nA,
      test_type = input$test_type
    )
  } else {
    two_mean_size(
      muA = input$muA,
      muB = input$muB,
      delta = input$delta,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      alpha = input$alpha,
      nB = input$nB,
      test_type = input$test_type
    )
  }
}
