main_ui_two_mean_size <- function(test_type) {
  withMathJax(
    tagList(
      h4("Result"),
      textOutput("result"),
      hr(),
      switch(test_type,
             "2-side" = tagList(
               h4("Add later"),
               p("Add later")
             ),
             "1-side" = tagList(
               h4("Add later"),
               p("Add later")
             ),
             "non-inferiority" = tagList(
               h4("Add later"),
               p("Add later")
             ),
             "equivalence" = tagList(
               h4("Add later"),
               p("Add later")
             )
      )
    )
  )
}
