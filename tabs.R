#### intro_tab ####
intro_tab <- tabItem(
  tabName = "intro_tab",
  h3("Introduction to local search algorithms"),
  p("This app is designed to help you understand the Steepest Ascent and Newton-Raphson local search algorithms used in statistical estimation.
    For simplicity, this app focusses on one-dimensional functions with one local maximum, namely the Poisson and Binomial functions"),
  h4("Local versus Global algorithms"),
  p("Optimization algorithms are divided into two categories; local and global algorithms. A local optimization algorithm is used when you know that the function contains a single optimum (see Figure A).
    A global optimization algorithm is used when there is little known about the structure of the function because it deals with multi-model distribtuions better (see Figure B).
    You may wonder, if global algorithms are more versatile, why use a local optimization algorithm at all? Besides that local search algorithms are computationally more straightforward,
    there is a guarantee that it will find the local optima when the assumptions are met,
    which is not the case for global search algorithms"),
  plotOutput("intro", height = "600px", width = "80%"),
  h4("Maximum likelihood estimation"),
  p("We use maximum likelihood estimation (MLE) to estimate the parameters of a statistical model given observations.
    MLE tries to find the parameter value that maximizes the likelihood of making the observations given the parameters.
    As in many statistical applications, we will maximize the log-likelihood; as finding the maximum value of the likelihood is the same as finding the maximum value of the log-likelihood
    (given that it is a monotonic transformation), but it is mathematically simpler because it turns multiplication into sums.")
)

#### steep_tab ####
accent_tab <- tabItem(
  tabName = "accent_tab",
  h3("Steepest Ascent"),
  p("Steepest Ascent is a local search algorithm, slowly moving to the local maxima"),
  plotOutput("accent", height = "600px", width = "80%")
)

#### new_raph_tab ####
newtraph_tab <- tabItem(
  tabName = "newtraph_tab",
  h3("Newton-Raphson Algorithm"),
  p("This algorithm is much faster than Steepest Ascent"),
  plotOutput("newtraph", height = "600px", width = "80%")
)
