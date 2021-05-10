### new_raph_tab ----
new_raph_tab <- tabItem(
  tabName = "new_raph_tab",
  h3("Newton-Raphson Algorithm"),
  p("This algorithm is much faster than Steepest Ascent"),
  plotOutput("new_raph_plot", height = "600px", width = "50%")
)
