## libraries ----
library(shiny)
library(shinydashboard) #Guides on how to use this are at: ttps://rstudio.github.io/shinydashboard/

## Functions ----
source("funcs.R")

## Tab pages ----
source("intro_tab.R")
source("steep_tab.R")
source("new_raph_tab.R")

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Local Search Algorithms"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro_tab"),
      menuItem("Steepest Ascent", tabName = "steep_tab"),
      menuItem("Newton-Raphson", tabName = "new_raph_tab"),
      box(
        title = "Variables",
        solidHeader = FALSE, width = NULL, background = "navy",
        # sliderInput("itrtns", "Iterations",
        #             min = 0, max = 100, value = 0, step = 1),
        actionButton("nextItter", "Next step"),
        actionButton("finish", "Finish"),
        actionButton("reset", "Reset"),
        selectInput("func", "Function",
                    choices = list("Poisson" = 1,
                                   "Binomial" = 2),
                    selected = 1),
        sliderInput("strtval", "Starting value", 1.5, min = 1.5, max = 5),
        numericInput("convval", "Convergence criterion", 1e-6, min = 1e-10, max = 1e-3)
      )
    )
  ),
  dashboardBody(
    tabItems(intro_tab,
             steep_tab,
             new_raph_tab)
  )
)

## UI ----
server <- function(input, output, session) {

  unlockBinding("invalidateLater", as.environment("package:shiny"))
  assign("invalidateLater", invalidateLaterNew, "package:shiny")

  #input data
  data <- c(2, 2, 3, 4, 4)
  n <- length(data)

  i <- reactiveVal(1)

  observeEvent(input$nextItter, {
    i(i() + 1)
  })

  observeEvent(input$reset, {
    i(1)
  })

  ##Plots ----
  output$steep_plot <- renderPlot({

    iterhist <- steep_accent(input$strtval, data, input$convval)

    # Make sure that i does not leave the plotting range (a bit of a dirty trick):
    if(i() > nrow(iterhist)){
      i(i() - 1)
    }

    # Define Poisson-Loglikelihood:
    loglikpois <- function(theta) {
      -n * theta + sum(data) * log(theta)
    }

    # Plot Poisson-Loglikelihood:
    plot(loglikpois, xlim = c(1.5, 4), ylim = c(-2, 2), lwd = 2,
         main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = "")

    # Draw point and slope dependent on selected iteration:
    points(iterhist$theta[i()],iterhist$ell[i()], col  = "blue", pch = 20, lwd = 3)
    # clip(-2, 2, -1, 1) # attempt to make abline shorter
    abline(iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()], iterhist$ellp[i()],
           col = "red", lwd = 2)

    if (i() == nrow(iterhist)) {
      text((1.5+4)/2, -2, "Converged")
    }
    text(4, -2, paste0("Iter: ", i()), pos = 2)

    legend(x = "topright",
           legend = c("f(x)", "f'(x)"),
           col = c("black", "red"),
           lwd = 2)

    # Attempt to plot delta:
    # plot(abs(iterhist$theta[i()+1]-iterhist$theta[i()]))

  })

  observeEvent(input$finish, {

    iterhist <- steep_accent(input$strtval, data, input$convval)

    # Define Poisson-Loglikelihood:
    loglikpois <- function(theta) {
      -n * theta + sum(data) * log(theta)
    }

    ii <- i()

    output$steep_plot <- renderPlot({

      ii <<- ii + 1
      invalidateLaterNew(100, session,  ii < nrow(iterhist))

      # Plot Poisson-Loglikelihood:
      plot(loglikpois, xlim = c(1.5, 4), ylim = c(-2, 2), lwd = 2,
           main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = "")

      # Draw point and slope dependent on selected iteration:
      points(iterhist$theta[ii],iterhist$ell[ii], col = "blue", pch = 20, lwd = 3)
      abline(iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii], iterhist$ellp[ii], col = "red", lwd = 2)
      if(ii == nrow(iterhist)){
        text((1.5+4) / 2, -2, "Converged")
      }
      text(4, -2, paste0("Iter: ", ii), pos = 2)

      legend(x = "topright",
             legend = c("f(x)", "f'(x)"),
             col = c("black", "red"),
             lwd = 2)

    })

    i(ii)
  })

  output$new_raph_plot <- renderPlot(plot_new_raph("poisson", data, i(), input$strtval))
}

shinyApp(ui, server)
