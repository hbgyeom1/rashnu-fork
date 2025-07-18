result_or_size <- function(input) {
  if (input$mode == "size") {
    or_size(
      pA = input$pA,
      pB = input$pB,
      delta = input$delta,
      kappa = input$kappa,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    or_size(
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
