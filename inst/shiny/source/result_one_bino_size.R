result_one_bino_size <- function(input) {
  if (input$mode == "size") {
    one_bino_size(
      p = input$p,
      p0 = input$p0,
      alpha = input$alpha,
      beta = input$beta
    )
  } else {
    one_bino_size(
      p = input$p,
      p0 = input$p0,
      alpha = input$alpha,
      n = input$n
    )
  }
}
