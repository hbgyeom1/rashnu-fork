result_k_mean_size <- function(input) {
  if (input$mode == "size") {
    k_mean_size(
      muA = input$muA,
      muB = input$muB,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      tau = input$tau,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else if (input$test_type == "1-side") {
    k_mean_size(
      muA = input$muA,
      muB = input$muB,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      tau = input$tau,
      alpha = input$alpha,
      nA = input$nA,
      test_type = input$test_type
    )
  } else {
    k_mean_size(
      muA = input$muA,
      muB = input$muB,
      kappa = input$kappa,
      sd = input$sd,
      sdA = input$sdA,
      sdB = input$sdB,
      tau = input$tau,
      alpha = input$alpha,
      n = input$n,
      test_type = input$test_type
    )
  }
}
