result_two_prop_size <- function(input) {
  if (input$mode == "size") {
    two_prop_size(
      pA = input$pA,
      pB = input$pB,
      delta = input$delta,
      kappa = input$kappa,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    two_prop_size(
      pA = input$pA,
      pB = input$pB,
      delta = input$delta,
      kappa = input$kappa,
      alpha = input$alpha,
      nB = input$nB,
      test_type = input$test_type
    )
  }
}
