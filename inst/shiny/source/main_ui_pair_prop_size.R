main_ui_pair_prop_size <- function(test_type) {
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
             )
      )
    )
  )
}
