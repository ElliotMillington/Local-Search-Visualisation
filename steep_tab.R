### steep_tab ----
steep_tab <- tabItem(
  tabName = "steep_tab",
  h3("Steepest Ascent"),
  p("Steepest Ascent is a local search algorithm, slowly moving to the local maxima"),
  plotOutput("steep_plot", height = "600px", width = "50%")
)

