#### intro_tab ####
intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Introduction"),
  p("This app is designed to help you understand the algorithms used in statistical estimation.")
)

#### steep_tab ####
accent_tab <- tabItem(
  tabName = "accent_tab",
  h3("Steepest Ascent"),
  p("Steepest Ascent is a local search algorithm, slowly moving to the local maxima"),
  plotOutput("accent", height = "600px", width = "50%")
)

#### new_raph_tab ####
newtraph_tab <- tabItem(
  tabName = "newtraph_tab",
  h3("Newton-Raphson Algorithm"),
  p("This algorithm is much faster than Steepest Ascent"),
  plotOutput("newtraph", height = "600px", width = "50%")
)
