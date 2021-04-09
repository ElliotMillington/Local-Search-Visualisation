
library(shiny)

ui <- fluidPage(
sliderInput(inputId="n", label="Iterations", min=0, max=5, value=0, step=1),
plotOutput("plot")
)


server <- function(input, output)
{
###########iteration process


#input data
dat <- c(2,2,3,4,4)
n <- length(dat)

#Newton Raphson algorithm
loglikpois <- function(theta){
  ll <- -n*theta+sum(dat)*log(theta)
  grad <- -n+sum(dat)/theta
  hess <- -sum(dat)/theta^2
  out<-list(ll=ll,grad=grad,hess=hess)
  return(out)
}

maxiter=500; delta=10^-6
th.vec=NULL; LL=NULL; LLp=NULL; LLp2=NULL
th.vec[1]=5

out<-loglikpois(th.vec[1])
LL[1]=out$ll; LLp[1]=out$grad; LLp2[1]=out$hess
convergence=FALSE
a=1
while ((!convergence)&(a<maxiter))
{
  th.vec[a+1] <- th.vec[a] - out$grad/out$hess
  out<-loglikpois(th.vec[a+1])
  LL[a+1]=out$ll; LLp[a+1]=out$grad; LLp2[a+1]=out$hess
  if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
  a <- a+1
}

iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp,ellp2=LLp2)
#iterhist could be displayed

######################plotting


output$plot <- renderPlot({

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
i=input$n+1
points(th.vec[i],LLp[i], col="blue", pch=20, lwd=5)
abline(LLp[i]-(LLp2[i]*th.vec[i]), LLp2[i], col="red", lwd=2) #abline(intercept, slope, color)
#abline(v=th.vec[i+1], lty=3, lwd=2, col="red")
clip(-5,5,0,LLp[i+1])
abline(v=th.vec[i+1], lty=3, lwd=3, col="red")
})
   

}

shinyApp(ui=ui, server=server)
