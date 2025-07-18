result_pair_prop_size <- function(input) {
  if (input$mode == "size") {
    pair_prop_size(
      p01 = input$p01,
      p10 = input$p10,
      alpha = input$alpha,
      beta = input$beta,
      test_type = input$test_type
    )
  } else {
    pair_prop_size(
      p01 = input$p01,
      p10 = input$p10,
      alpha = input$alpha,
      n = input$n,
      test_type = input$test_type
    )
  }
}
