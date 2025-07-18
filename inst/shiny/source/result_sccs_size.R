result_sccs_size <- function(input) {
  if (input$mode == "size") {
    sccs_size(
      p = input$p,
      r = input$r,
      alpha = input$alpha,
      beta = input$beta
    )
  } else {
    sccs_size(
      p = input$p,
      r = input$r,
      alpha = input$alpha,
      n = input$n
    )
  }
}
