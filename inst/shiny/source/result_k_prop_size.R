result_k_prop_size <- function(input) {
  if (input$mode == "size") {
    k_prop_size(
      pA = input$pA,
      pB = input$pB,
      tau = input$tau,
      alpha = input$alpha,
      beta = input$beta
    )
  } else {
    k_prop_size(
      pA = input$pA,
      pB = input$pB,
      tau = input$tau,
      alpha = input$alpha,
      n = input$n
    )
  }
}
