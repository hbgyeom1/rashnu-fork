result_one_mean_size <- function(input) {
  if (input$mode == "size") {
    one_mean_size(
      mu = input$mu,
      mu0 = input$mu0,
      delta = input$delta,
      sd = input$sd,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    one_mean_size(
      mu = input$mu,
      mu0 = input$mu0,
      delta = input$delta,
      sd = input$sd,
      alpha = input$alpha,
      n = input$n,
      test_type = input$test_type
    )
  }
}
