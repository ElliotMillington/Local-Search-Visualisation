# Log likelihood Poisson:
loglikpois <- function(data, theta, n) {
  ll   <- -n * theta + sum(data) * log(theta)
  grad <- -n + sum(data) / theta
  hess <- -sum(data) / theta^2
  out  <- list(ll = ll, grad = grad, hess = hess)
  return(out)
}

# Log likelihood Binomial:
loglikbinom <- function(theta, n = 10, successes = 7){
  ll <- successes*log(theta)+(n-successes)*log(1-theta)
  grad <- successes/theta - (n-successes)/(1-theta)
  hess <- -successes/theta^2 - (n-successes)/(1-theta)^2
  out <- list(ll=ll, grad=grad, hess=hess)
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

# plot_steep <- function(data, strt, convval) {
#   iterhist <- steep_accent(strtval, data, convval)
#   loglik <- switch(distribution,
#                    "poisson" = poisson_lik)
#
#   if(i() > nrow(iterhist)){
#     i(i() - 1)
#   }
#
#   x <- seq(1.5, 4, by = 0.1)
#   y <- loglikpois(x, data)
#
#   plot(loglikpois, xlim = c(1.5, 4), ylim = c(-2, 2), lwd = 2,
#        main = "The Steepest-Ascent Algorithm", ylab = "Log Likelihood", xlab = "")
# }
#
steep_accent <- function(strtval, data, delta, maxiter = 500, alpha = 0.015,
                         LL = NULL, LLp = NULL, distribution = 1, successes = 7){
  th.vec <- strtval

  if (distribution == 1) {
    n <- length(data)
    out <- loglikpois(data, th.vec[1], n)
  } else {
    out <- loglikbinom(theta = th.vec[1], n = 10, successes = successes)
  }

  LL[1] <- out$ll; LLp[1] <- out$grad
  convergence <- FALSE
  i <- 1
  while ((!convergence) & (i < maxiter))
  {
    th.vec[i+1] <- th.vec[i] + alpha*out$grad
    if (distribution == 1){
      out <- loglikpois(data, th.vec[i+1], n)
    } else {
      out <- loglikbinom(theta = th.vec[i+1], successes = successes)
    }
    LL[i+1] <- out$ll; LLp[i+1] <- out$grad
    if ((abs(th.vec[i+1]-th.vec[i])) < delta) { convergence <- TRUE }
    i <- i+1
  }

  iterhist = data.frame(t = seq(1, i), theta = th.vec, ell = LL, ellp = LLp)
  return(iterhist)
}

plot_new_raph <- function(distribution, data = c(2,2,3,4,4), i, strt) {
    hist <- new_raph(data, "poisson", strt)

    n <- length(data)

    th.vec <- hist$theta
    LL <- hist$ell
    LLp <- hist$ellp
    LLp2 <- hist$ellp2

    #funcs <- loglikfun(distribution)
    loglik <- poisson_lik #funcs$loglik
    grad <- poisson_lik_dv #funcs$loglikdv

    x <- seq(1.5, 5.5, 0.1)
    y <- loglik(x, data)
    y_dx <- grad(x, data)

    par(mfrow = c(2, 1))

    plot(x, y, type = "l", xlim = c(1.5, 5.5), ylim = c(-3, 4.5), lwd = 2,
         main = "The Newton-Raphson-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))
    lines(x, y_dx, col = "red")
    abline(h = 0, lty = 2)
    legend(x = "topright",
           legend = c("f(x)", "f'(x)", "f''(x)"),
           col = c("black", "red", "green"),
           lwd = 2)

    if(i > nrow(hist)){
      i <- nrow(hist)
    }

    ##draw point and slope dependent on selected iteration
    text(1.5, -3, paste("Iter: ", i, if(i == nrow(hist)) "(Converged)"), pos = 4)
    points(th.vec[i], LLp[i], col = "blue", pch = 20, lwd = 5)
    #clip(LLp[i]-1, LLp[i]+1, LLp[i]-1, LLp[i]+1)
    abline(LLp[i]-(LLp2[i]*th.vec[i]), LLp2[i], col = "green", lwd = 2) #abline(intercept, slope, color)
    #abline(v=th.vec[i+1], lty=3, lwd=2, col="green")
    clip(-5,5,0,LLp[i])
    abline(v=th.vec[i], lty = 2, lwd = 2, col = "green")


    # Plot the iteration history:
    plot(1:i, th.vec[1:i], xlim = c(1, i), ylim = c(1.5, 4), lwd = 2, type = "b",
         main = "Iteration", ylab = "Parameter Estimate", xlab = "")
    abline(h = mean(data), lty = 3)
}

new_raph <- function(data, distribution, start_pos) {
  loglikpois_df <- switch(distribution,
                       "poisson" = poisson_lik_df)
  # Newton-Raphson algorithm
  maxiter=500; delta=10^-6
  th.vec=NULL; LL=NULL; LLp=NULL; LLp2=NULL
  th.vec[1]=start_pos

  out <- loglikpois_df(th.vec[1], data)
  LL[1] = out$ll; LLp[1] = out$grad; LLp2[1] = out$hess
  convergence=FALSE
  a=1
  while ((!convergence)&(a<maxiter))
  {
    th.vec[a+1] <- th.vec[a] - out$grad/out$hess
    out<-loglikpois_df(th.vec[a+1], data)
    LL[a+1]=out$ll; LLp[a+1]=out$grad; LLp2[a+1]=out$hess
    if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
    a <- a+1
  }

  iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp,ellp2=LLp2)
}

poisson_lik <- function(theta, data) {
  n <- length(data)
  -n*theta+sum(data)*log(theta)
}

poisson_lik_dv <- function(theta, data) {
  n <- length(data)
  -n+sum(data)/theta
}

poisson_lik_df <- function(theta, data) {
  n <- length(data)
  ll <- poisson_lik(theta, data)
  grad <- -n+sum(data)/theta
  hess <- -sum(data)/theta^2
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
