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
        conditionalPanel("input.func==1",{
          sliderInput("strtval", "Starting value", 1.5, min = 1.5, max = 5)
        }),
        conditionalPanel("input.func==2",{
          sliderInput("strtval2", "Starting value", .5, min = 0, max = 1)
        }),
        conditionalPanel("input.func==2",{
          numericInput("successes", "Success out of 10", 7, min = 1, max = 9)
        }),
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

  # Input data:
  data <- c(2, 2, 3, 4, 4)
  n <- length(data)
  n1 <- 10

  i <- reactiveVal(1)

  observeEvent(input$nextItter, {
    i(i() + 1)
  })

  observeEvent(input$reset, {
    i(1)
  })

  Condition <- reactive({input$func == 1})

    ##Plots ----
    output$steep_plot <- renderPlot({
      if (Condition()) {
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
             main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))

        # Draw point and slope dependent on selected iteration:
        points(iterhist$theta[i()], iterhist$ell[i()], col  = "blue", pch = 20, lwd = 3)

        # Short line:
        segments(x0 = iterhist$theta[i()]-0.35,
                 y0 = (iterhist$theta[i()]-0.35)*iterhist$ellp[i()]+iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()],
                 x1 = iterhist$theta[i()]+0.35,
                 y1 = (iterhist$theta[i()]+0.35)*iterhist$ellp[i()]+iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()],
                 col = "red", lwd = 2)

        if (i() == nrow(iterhist)) {
          text((1.5+4)/2, -2, "Converged")
        }
        text(4, -2, paste0("Iter: ", i()), pos = 2)

        legend(x = "topright",
               legend = c("f(x)", "f'(x)"),
               col = c("black", "red"),
               lwd = 2)

      } else {
          successes <- input$successes
          iterhist <- steep_accent(input$strtval2, data, input$convval,distribution = input$func, successes = input$successes)

          # Make sure that i does not leave the plotting range (a bit of a dirty trick):
          if(i() > nrow(iterhist)){
            i(i() - 1)
          }

          # Define Binomial-Loglikelihood:
          loglikbinom <- function(theta, successes = input$successes, n1 = 10){
            successes*log(theta)+(n1-successes)*log(1-theta)
          }

          # Plot Binomial-Loglikelihood:
          plot(loglikbinom, xlim = c(0, 1), ylim = c(-35, 10), lwd = 2,
               main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))

          # Draw point and slope dependent on selected iteration:
          points(iterhist$theta[i()], iterhist$ell[i()], col  = "blue", pch = 20, lwd = 3)

          # Short line:
          segments(x0 = iterhist$theta[i()]-0.1,
                   y0 = (iterhist$theta[i()]-0.1)*iterhist$ellp[i()]+iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()],
                   x1 = iterhist$theta[i()]+0.1,
                   y1 = (iterhist$theta[i()]+0.1)*iterhist$ellp[i()]+iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()],
                   col = "red", lwd = 2)

          if (i() == nrow(iterhist)) {
            text(.5, -35, "Converged")
          }
          text(.9, -35, paste0("Iter: ", i()), pos = 2)

          legend(x = "topright",
                 legend = c("f(x)", "f'(x)"),
                 col = c("black", "red"),
                 lwd = 2)

      }
    })

    observeEvent(input$finish, {

      if (Condition()) {
        iterhist <- steep_accent(input$strtval, data, input$convval)
      } else{
        iterhist <- steep_accent(input$strtval2, data, input$convval, distribution = input$func, successes = input$successes)
        successes <- input$successes
      }

      # Define Poisson-Loglikelihood:
      loglikpois <- function(theta) {
        -n * theta + sum(data) * log(theta)
      }

      # Define Binomial-Loglikelihood:
      loglikbinom <- function(theta, successes = input$successes, n1 = 10){
        successes*log(theta)+(n1-successes)*log(1-theta)
      }
      par(mfrow = c(2, 1))

      # Plot Poisson-Loglikelihood:
      plot(loglikpois, xlim = c(1.5, 4), ylim = c(-2, 2), lwd = 2,
           main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = "")

      ii <- i()

      output$steep_plot <- renderPlot({
        if(Condition()){
          ii <<- ii + 1
          invalidateLaterNew(100, session,  ii < nrow(iterhist))

          # Plot Poisson-Loglikelihood:
          plot(loglikpois, xlim = c(1.5, 4), ylim = c(-2, 2), lwd = 2,
               main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))

          # Draw point and slope dependent on selected iteration:
          points(iterhist$theta[ii],iterhist$ell[ii], col = "blue", pch = 20, lwd = 3)
          #abline(iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii], iterhist$ellp[ii], col = "red", lwd = 2)
          segments(x0=iterhist$theta[ii]-0.35,
                   y0=(iterhist$theta[ii]-0.35)*iterhist$ellp[ii]+iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii],
                   x1=iterhist$theta[ii]+0.35,
                   y1=(iterhist$theta[ii]+0.35)*iterhist$ellp[ii]+iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii],
                   col = "red", lwd = 2)

          if (ii == nrow(iterhist)) {
            text((1.5+4) / 2, -2, "Converged")
          }
          text(4, -2, paste0("Iter: ", ii), pos = 2)

          legend(x = "topright",
                 legend = c("f(x)", "f'(x)"),
                 col = c("black", "red"),
                 lwd = 2)
      } else {
        ii <<- ii + 1
        invalidateLaterNew(100, session,  ii < nrow(iterhist))

        # Plot Binomial-Loglikelihood:
        plot(loglikbinom, xlim = c(0, 1), ylim = c(-35, 0), lwd = 2,
             main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))

        # Draw point and slope dependent on selected iteration:
        points(iterhist$theta[ii], iterhist$ell[ii], col = "blue", pch = 20, lwd = 3)
        #abline(iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii], iterhist$ellp[ii], col = "red", lwd = 2)
        segments(x0=iterhist$theta[ii]-0.1,y0=(iterhist$theta[ii]-0.1)*iterhist$ellp[ii]+iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii],
                 x1=iterhist$theta[ii]+0.1,y1=(iterhist$theta[ii]+0.1)*iterhist$ellp[ii]+iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii],
                 col = "red", lwd = 2)

        if(ii == nrow(iterhist)){
          text(.5, -35, "Converged")
        }
        text(.9, -35, paste0("Iter: ", ii), pos = 2)

        legend(x = "topright",
               legend = c("f(x)", "f'(x)"),
               col = c("black", "red"),
               lwd = 2)
      }

      })

      # Plot the iteration history:
      plot(iterhist$t[1:ii], iterhist$theta[1:ii], xlim = c(1, ii), ylim = c(1.5, 4), lwd = 2, type = "b",
           main = "Iteration", ylab = "Parameter Estimate", xlab = "")
      abline(h = mean(data), lty = 3)

    })

    output$new_raph_plot <- renderPlot(plot_new_raph("poisson", data, i(), input$strtval))


}

shinyApp(ui, server)
