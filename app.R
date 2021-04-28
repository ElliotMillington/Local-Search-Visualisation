## libraries ----
library(shiny)
library(shinydashboard)

## Functions ----
source("funcs.R")
# making the animation work
# https://stackoverflow.com/questions/43337147/r-shiny-adding-to-plot-via-a-loop/43344162#43344162
invalidateLaterNew <- function (millis, session = getDefaultReactiveDomain(), update = TRUE) {
  if(update){
    ctx <- shiny:::.getReactiveEnvironment()$currentContext()
    shiny:::timerCallbacks$schedule(millis, function() {
      if (!is.null(session) && session$isClosed()) {
        return(invisible())
      }
      ctx$invalidate()
    })
    invisible()
  }
}

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
        selectInput("func", "Function",
                    choices = list("Poisson" = 1,
                                   "Binomial" = 2),
                    selected = 1),
        numericInput("strtval", "Starting value", 1.5, min = 1.5, max = 4),
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
  dat <- c(2,2,3,4,4)
  n <- length(dat)
  i <- reactiveVal(1)

  observeEvent(input$nextItter, {
    i(i() + 1)
  })

  ##Plots ----
  output$steep_plot <- renderPlot({

    iterhist <- steep_accent(input$strtval, dat, input$convval)

    # make sure that i does not leave the plotting range (a bit of a dirty trick)
    if(i() > nrow(iterhist)){
      i(i() - 1)
    }

    #define Poisson-Loglikelihood
    loglikpois <- function(theta) {
      -n*theta+sum(dat)*log(theta)
    }

    #Plot Poisson-Loglikelihood
    plot(loglikpois, xlim=c(1.5,4), ylim=c(-2,2), lwd=2, main="The Steepest-Ascent-Algorithm")
    ###draw point and slope dependent on selected iteration
    points(iterhist$theta[i()],iterhist$ell[i()], col="blue", pch=20, lwd=3)
    abline(iterhist$ell[i()]-iterhist$ellp[i()]*iterhist$theta[i()], iterhist$ellp[i()], col="red", lwd=2)
    if(i() == nrow(iterhist)){
      text((1.5+4)/2, -2, "Converged")
    }
    text(4, -2, paste0("Iter: ", i()), pos = 2)

    #abline(intercept, slope, color, linewidth)

  })
  observeEvent(input$finish, {

    iterhist <- steep_accent(input$strtval, dat, input$convval)
    #define Poisson-Loglikelihood
    loglikpois <- function(theta) {
      -n*theta+sum(dat)*log(theta)
    }

    ii <- i()

    output$steep_plot <- renderPlot({

      ii <<- ii + 1
      invalidateLaterNew(100, session,  ii < nrow(iterhist))

      plot(loglikpois, xlim=c(1.5,4), ylim=c(-2,2), lwd=2, main="The Steepest-Ascent-Algorithm")
      ###draw point and slope dependent on selected iteration
      points(iterhist$theta[ii],iterhist$ell[ii], col="blue", pch=20, lwd=3)
      abline(iterhist$ell[ii]-iterhist$ellp[ii]*iterhist$theta[ii], iterhist$ellp[ii], col="red", lwd=2)
      if(ii == nrow(iterhist)){
        text((1.5+4)/2, -2, "Converged")
      }
      text(4, -2, paste0("Iter: ", ii), pos = 2)
    })

    i(ii)
  })


  output$new_ralph_plot <- renderPlot({

    #define Poisson-Loglikelihood
    loglikpois <- function(theta) {
      -n*theta+sum(dat)*log(theta)
    }

    #define first derivative of Poisson-Loglikelihood
    grad <- function(theta) {-n+sum(dat)/theta}



    #Plot Poisson-Loglikelihood and its first derivative
    plot(loglikpois, ylab="y", xlim=c(1.5,5.5), ylim=c(-3,4.5), lwd=2, main="The Newton-Raphson-Algorithm")
    curve(grad, from=1.5, to=5.5, lwd=2, col="green", add=T)
    abline(h=0, lty=2)
    legend(x = "topright",
           legend = c("f(x)", "f'(x)"),
           col = c("black", "green"),
           lwd = 2)

    ###draw point and slope dependent on selected iteration
    i=input$itrtns+1
    points(th.vec[i()],LLp[i()], col="blue", pch=20, lwd=5)
    abline(LLp[i()]-(LLp2[i()]*th.vec[i()]), LLp2[i()], col="red", lwd=2) #abline(intercept, slope, color)
    #abline(v=th.vec[i+1], lty=3, lwd=2, col="red")
    clip(-5,5,0,LLp[i()+1])
    abline(v=th.vec[i()+1], lty=3, lwd=3, col="red")
  })
}

shinyApp(ui, server)
