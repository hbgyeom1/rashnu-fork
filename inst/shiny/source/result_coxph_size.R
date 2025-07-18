result_coxph_size <- function(input) {
  if (input$mode == "size") {
    coxph_size(
      hr = input$hr,
      hr0 = input$hr0,
      pE = input$pE,
      pA = input$pA,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    coxph_size(
      hr = input$hr,
      hr0 = input$hr0,
      pE = input$pE,
      pA = input$pA,
      alpha = input$alpha,
      n = input$n,
      test_type = input$test_type
    )
  }
}
