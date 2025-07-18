result_one_norm_size <- function(input) {
  if (input$mode == "size") {
    one_norm_size(
      mu = input$mu,
      mu0 = input$mu0,
      sd = input$sd,
      alpha = input$alpha,
      beta = input$beta
    )
  } else {
    one_norm_size(
      mu = input$mu,
      mu0 = input$mu0,
      sd = input$sd,
      alpha = input$alpha,
      n = input$n
    )
  }
}
