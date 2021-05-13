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
  withMathJax(),
  h3("Steepest Ascent"),
  # p("Steepest Ascent is a local search algorithm, slowly moving to the local maxima"),
  p("The steepest ascent algorithm is an iterative optimization algorithm to find the maximum of the likelihood function. This algorithm consists of taking repeated steps in the direction of the first derivative. Given a point \\(\\theta^{t-1}\\) the next step of the algorithm \\(\\theta^{t}\\) is obtained by the following updating rule:"),
  p("$$\\theta^{t}=\\theta^{t-1} - \\alpha f'(\\theta^{t-1}) $$"),
  p("where \\(\\alpha\\) a smaller constant chooses to scale the first derivative."),
  p("In the ShinyApp the user can select one of the two likelihood functions, namely,
    Poisson and binomial. The three sliders on the left allow to set the starting point \\(\\theta^{t-1}\\)
    and the parameters of the function (i.e., mean and the number of observations for
    the Poisson and number of observations and successes for the binomial).
    The convergence criterion is the termination criterion for the algorithm.
    The ", strong("Reset") ," button allows restarting the visualization after the change of any of these parameters. "),
  p("The ",strong("Next step")," button allows the user to visualize each step of the procedure.
     In the upper figure, the black line is the log-likelihood.
     The current point \\(\\theta^{t-1}\\) is represented by the ", span("blue dot",style= "color:blue"),", the continuous ", span("red segment",style= "color:red")," is the local approximation, the ", span("grey dot",style= "color:grey")," is the estimation of $\theta$ at the next step and finally, the vertical dashed lines indicate the maximum of the likelihood function.
     In the lower figure, on the x-axis, there are iterations and the black circles are the estimation of \\(\\theta\\) at that step. The green horizontal line is the true value of \\(\\theta\\)
     Finally, the", strong("Finish") ," button allows the algorithm to continue until the algorithm converges or the maximum number of iterations is reached."),

  plotOutput("accent", height = "600px", width = "80%")
)

#### new_raph_tab ####
newtraph_tab <- tabItem(
  tabName = "newtraph_tab",
  h3("Newton-Raphson Algorithm"),
  withMathJax(),
  h3("Newton-Raphson Algorithm"),
  #p("This algorithm is much faster than Steepest Ascent"),
  p("In the Newton-Raphson algorithm, the likelihood function is locally approximated as a second-order Taylor expansion, then the algorithm computes the maximum of this expansion. Given a point \\(\\theta^{t-1}\\) the maximum of the second-order Taylor expansion of the log-likelihood is"),
  p("$$\\frac{f'(\\theta^{t-1})}{f''(\\theta^{t-1})}.$$"),
  p("The algorithm proceeds in the direction of the maximum and the point \\(\\theta^{t}\\) is obtained by the following updating rule"),
  p("$$\\theta^{t}=\\theta^{t-1} - \\frac{f'(\\theta^{t-1})}{f''(\\theta^{t-1})}.$$"),
  p("In the ShinyApp the user can select one of the two likelihood functions, namely, Poisson and binomial.
    The three sliders on the left allow to set the starting point \\(\\theta^{t-1}\\) and the parameters of the
    function (i.e., mean and the number of observations for the Poisson and number of observations and successes for
    the binomial). The convergence criterion is the termination criterion for the algorithm.
    The ", strong("Reset") ," button allows to restart the visualization after the change of any of
    these parameters. "),
  p("The ",strong("Next step")," button allows the user to visualize each step of the procedure.
    In the upper figure, the black line is the log-likelihood,
    and the ", span("red line",style= "color:red")," is the first derivative of the log-likelihood."),
  p("The current point \\(\\theta^{t-1}\\) is represented by the ", span("blue dot",style= "color:blue"),", the continuous ", span("green segment",style= "color:green")," is the local approximation of the first derivative,
    the ", span("grey dot",style= "color:grey")," is the estimation of \\(\\theta\\) at the next step and finally,
    the vertical dashed lines indicate the maximum of the likelihood function.
    In the lower figure, on the x-axis, there are iterations and the black circles are the estimation of \\(\\theta\\) at that step. The green horizontal line is the true value of \\(\\theta\\) "),
  p("Finally, the ", strong("Finish") ," button allows the algorithm to continue until the algorithm converges or the maximum number of iterations is reached."),

  plotOutput("newtraph", height = "600px", width = "80%")
)
