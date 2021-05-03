#Log likelihood algorithm
loglikpois <- function(dat, theta, n){
  ll <- -n*theta+sum(dat)*log(theta)
  grad <- -n+sum(dat)/theta
  hess <- -sum(dat)/theta^2
  out<-list(ll=ll,grad=grad,hess=hess)
  return(out)
}

# loglikfun <- function(distribution) {
#   out <- c()
#   if (distribution == "poisson") {
#     out$loglik <- poisson_lik
#     out$loglikdv <- poisson_lik_df
#   }
#   return(out)
# }

plot_steep <- function(dat, strt, convval) {
  iterhist <- steep_accent(strtval, dat, convval)
  loglik <- switch(distribution,
                   "poisson" = poisson_lik)
  
  if(i() > nrow(iterhist)){
    i(i() - 1)
  }
  
  x <- seq(1.5, 4, by = 0.1)
  y <- loglikpois(x, dat)
  
  plot(loglikpois, xlim=c(1.5,4), ylim=c(-2,2), lwd=2, main="The Steepest-Ascent-Algorithm")
}

steep_accent <- function(strtval, dat, delta, maxiter=500, alpha=.015,
                         LL=NULL, LLp=NULL){

  th.vec <- strtval
  n <- length(dat)

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
  return(iterhist)
}

plot_new_raph <- function(distribution, dat=c(2,2,3,4,4), i, strt) {
    hist <- new_raph(dat, "poisson", strt)
    
    n <- length(dat)
    
    th.vec <- hist$theta
    LL <- hist$ell
    LLp <- hist$ellp
    LLp2 <- hist$ellp2
    
    #funcs <- loglikfun(distribution)
    loglik <- poisson_lik #funcs$loglik
    grad <- poisson_lik_dv #funcs$loglikdv
    
    x <- seq(1.5, 5.5, 0.1)
    y <- loglik(x, dat)
    y_dx <- grad(x, dat)
    
    plot(x, y, type="l", ylab="y", xlim=c(1.5,5.5), ylim=c(-3,4.5), lwd=2, 
         main="The Newton-Raphson-Algorithm")
    lines(x, y_dx, col="green")
    abline(h=0, lty=2)
    legend(x = "topright",
           legend = c("f(x)", "f'(x)"),
           col = c("black", "green"),
           lwd = 2)
    
    if(i > nrow(hist)){
      i <- i - 1
    }
    
    ##draw point and slope dependent on selected iteration
    text(2, -2, paste("Iter: ", i), pos = 2)
    points(th.vec[i],LLp[i], col="blue", pch=20, lwd=5)
    abline(LLp[i]-(LLp2[i]*th.vec[i]), LLp2[i], col="red", lwd=2) #abline(intercept, slope, color)
    #abline(v=th.vec[i+1], lty=3, lwd=2, col="red")
    clip(-5,5,0,LLp[i])
    abline(v=th.vec[i], lty=3, lwd=3, col="red")
}

new_raph <- function(dat, distribution, start_pos) {
  loglikpois_df <- switch(distribution,
                       "poisson" = poisson_lik_df)
  
  # Newton-Raphson algorithm
  maxiter=500; delta=10^-6
  th.vec=NULL; LL=NULL; LLp=NULL; LLp2=NULL
  th.vec[1]=start_pos
  
  out<-loglikpois_df(th.vec[1], dat)
  LL[1]=out$ll; LLp[1]=out$grad; LLp2[1]=out$hess
  convergence=FALSE
  a=1
  while ((!convergence)&(a<maxiter))
  {
    th.vec[a+1] <- th.vec[a] - out$grad/out$hess
    out<-loglikpois_df(th.vec[a+1], dat)
    LL[a+1]=out$ll; LLp[a+1]=out$grad; LLp2[a+1]=out$hess
    if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
    a <- a+1
  }
  
  iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp,ellp2=LLp2)
}

poisson_lik <- function(theta, dat) {
  n <- length(dat)
  -n*theta+sum(dat)*log(theta)
}

poisson_lik_dv <- function(theta, dat) {
  n <- length(dat)
  -n+sum(dat)/theta
}

poisson_lik_df <- function(theta, dat) {
  n <- length(dat)
  ll <- poisson_lik(theta, dat)
  grad <- -n+sum(dat)/theta
  hess <- -sum(dat)/theta^2
  out<-list(ll=ll,grad=grad,hess=hess)
  return(out)
}

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