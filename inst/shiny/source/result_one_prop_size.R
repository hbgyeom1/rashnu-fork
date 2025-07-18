result_one_prop_size <- function(input) {
  if (input$mode == "size") {
    one_prop_size(
      p = input$p,
      p0 = input$p0,
      delta = input$delta,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    one_prop_size(
      p = input$p,
      p0 = input$p0,
      delta = input$delta,
      alpha = input$alpha,
      n = input$n,
      test_type = input$test_type
    )
  }
}
