library(shiny)

ui <- fluidPage(
  sliderInput(inputId="n", label="Iterations", min=0, max=100, value=0, step=10),
  plotOutput("plot")
)


server <- function(input, output)
{
  ###########iteration process
  
  
  #input data
  dat <- c(2,2,3,4,4)
  n <- length(dat)
  
  #steepest ascent algorithm
  loglikpois <- function(theta){
    ll <- -n*theta+sum(dat)*log(theta)
    grad <- -n+sum(dat)/theta
    hess <- -sum(dat)/theta^2
    out<-list(ll=ll,grad=grad,hess=hess)
    return(out)
  }
  
  maxiter=500; alpha=.015; delta=10^-6
  th.vec=NULL; LL=NULL; LLp=NULL
  th.vec[1]=2
  
  out<-loglikpois(th.vec[1])
  LL[1]=out$ll; LLp[1]=out$grad
  convergence=FALSE
  a=1
  while ((!convergence)&(a<maxiter))
  {
    th.vec[a+1] <- th.vec[a] + alpha*out$grad
    out<-loglikpois(th.vec[a+1])
    LL[a+1]=out$ll; LLp[a+1]=out$grad
    if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
    a <- a+1
  }
  
  iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp)
  #iterhist could be displayed
  
  ######################plotting
  
  
  output$plot <- renderPlot({
    
    #define Poisson-Loglikelihood
    loglikpois <- function(theta) {
      -n*theta+sum(dat)*log(theta)
    }
    
    #Plot Poisson-Loglikelihood
    plot(loglikpois, xlim=c(1.5,4), ylim=c(-2,2), lwd=2, main="The Steepest-Ascent-Algorithm")
    
    ###draw point and slope dependent on selected iteration
    i=input$n+1
    points(th.vec[i],LL[i], col="blue", pch=20, lwd=3)
    abline(LL[i]-(LLp[i]*th.vec[i]), LLp[i], col="red", lwd=2) #abline(intercept, slope, color, linewidth)
    
  })
  
  
}

shinyApp(ui=ui, server=server)