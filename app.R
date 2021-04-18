## libraries ----
library(shiny)
library(shinydashboard)

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
        sliderInput("itrtns", "Iterations",
                    min = 0, max = 100, value = 0, step = 1),
        selectInput("func", "Function",
                    choices = list("2x - x^2" = 1,
                                   "-x^4 + 6x^3 - 11x^2 + 6x" = 2),
                    selected = 1)
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
  ##Steepest data prep ----
  #input data
  dat <- c(2,2,3,4,4)
  n <- length(dat)
  
  #steepest ascent algorithm
  maxiter=500; alpha=.015; delta=10^-6
  th.vec=NULL; LL=NULL; LLp=NULL
  th.vec[1]=2
  
  out<-loglikpois(dat, th.vec[1], n)
  LL[1]=out$ll; LLp[1]=out$grad
  convergence=FALSE
  a=1
  while ((!convergence)&(a<maxiter))
  {
    th.vec[a+1] <- th.vec[a] + alpha*out$grad
    out<-loglikpois(dat, th.vec[a+1], n)
    LL[a+1]=out$ll; LLp[a+1]=out$grad
    if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
    a <- a+1
  }
  
  iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp)
  #iterhist could be displayed  
  
  ##Plots ----
  output$steep_plot <- renderPlot({
    
    #define Poisson-Loglikelihood
    loglikpois <- function(theta) {
      -n*theta+sum(dat)*log(theta)
    }
    
    #Plot Poisson-Loglikelihood
    plot(loglikpois, xlim=c(1.5,4), ylim=c(-2,2), lwd=2, main="The Steepest-Ascent-Algorithm")
    
    ###draw point and slope dependent on selected iteration
    i=input$itrtns+1
    points(th.vec[i],LL[i], col="blue", pch=20, lwd=3)
    abline(LL[i]-(LLp[i]*th.vec[i]), LLp[i], col="red", lwd=2) #abline(intercept, slope, color, linewidth)
    
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
    points(th.vec[i],LLp[i], col="blue", pch=20, lwd=5)
    abline(LLp[i]-(LLp2[i]*th.vec[i]), LLp2[i], col="red", lwd=2) #abline(intercept, slope, color)
    #abline(v=th.vec[i+1], lty=3, lwd=2, col="red")
    clip(-5,5,0,LLp[i+1])
    abline(v=th.vec[i+1], lty=3, lwd=3, col="red")
  })
}

shinyApp(ui, server)