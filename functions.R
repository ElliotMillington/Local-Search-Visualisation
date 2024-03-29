#### Helpers ####
t_likelihood  <- function(input){
  switch(
    input$func,
    "1" = "pois",
    "2" = "binom"
  )
}
t_data        <- function(input){
  switch(
    t_likelihood(input),
    "pois"  = list(
      n    = if(is.null(input$pois_n))    10  else input$pois_n,
      mean = if(is.null(input$pois_mean)) 2.5 else input$pois_mean
    ),
    "binom" = list(
      n         = if(is.null(input$binom_failures) | is.null(input$binom_successes))  10 else input$binom_successes + input$binom_failures,
      successes = if(is.null(input$binom_successes))  3 else input$binom_successes)
  )
}
t_start_pos   <- function(input){
  switch(
    t_likelihood(input),
    "pois"  = if(is.null(input$pois_start_val))  1.5 else input$pois_start_val,
    "binom" = if(is.null(input$binom_start_val))  .5 else input$binom_start_val
  )
}
t_criterion   <- function(input){
  return(if(is.null(input$criterion)) 1e-5 else input$criterion)
}

#### Likelihoods ####
# Log likelihood Poisson:
loglikpois <- function(theta, n, mean) {
  ll   <- -n * theta + mean * n * log(theta)
  grad <- -n + mean * n / theta
  hess <- - mean * n / theta^2
  out  <- list(ll = ll, grad = grad, hess = hess)
  return(out)
}

# Log likelihood Binomial:
loglikbinom <- function(theta, n, successes){
  ll   <- successes*log(theta)+(n-successes)*log(1-theta)
  grad <- successes/theta - (n-successes)/(1-theta)
  hess <- -successes/theta^2 - (n-successes)/(1-theta)^2
  out  <- list(ll = ll, grad = grad, hess = hess)
  return(out)
}

# Wrapper for choosing the likelihood
get_likelihood <- function(likelihood){
  switch(
    likelihood,
    "pois"   = loglikpois,
    "binom"  = loglikbinom
  )
}

#### Algorithms ####
# Steepest accent algorithm
alg_accent   <- function(likelihood, data, start_pos, criterion, alpha = 0.015, maxiter = 500){

  # initiate the algorithm
  args       <- data
  args$theta <- start_pos
  fun        <- get_likelihood(likelihood)
  out        <- do.call(fun, args)

  th.vec      <- start_pos
  LL          <- out$ll
  LLp         <- out$grad
  convergence <- FALSE

  # run the algorithm
  i <- 1
  while (!convergence & (i < maxiter)){

    th.vec[i+1] <- th.vec[i] + alpha*out$grad
    args$theta  <- th.vec[i+1]
    out         <- do.call(fun, args)

    LL[i+1]     <- out$ll
    LLp[i+1]    <- out$grad
    convergence <- (abs(th.vec[i+1]-th.vec[i])) < criterion

    i <- i + 1
  }

  return(list(
    t     = seq(1, i),
    theta = th.vec,
    ell   = LL,
    ellp  = LLp,
    fun   = fun,
    data  = data,
    likelihood = likelihood
  ))
}

# Newton-Raphson algorithm
alg_newtraph <- function(likelihood, data, start_pos, criterion, delta = 10^-6, maxiter = 500){

  # initiate the algorithm
  args       <- data
  args$theta <- start_pos
  fun        <- get_likelihood(likelihood)
  out        <- do.call(fun, args)

  th.vec      <- start_pos
  LL          <- out$ll
  LLp         <- out$grad
  LLp2        <- out$hess
  convergence <- FALSE

  # run the algorithm
  i <- 1
  while(!convergence & (i < maxiter)){

    th.vec[i+1] <- th.vec[i] - out$grad/out$hess
    args$theta  <- th.vec[i+1]
    out         <- do.call(fun, args)

    LL[i+1]     <- out$ll
    LLp[i+1]    <- out$grad
    LLp2[i+1]   <- out$hess
    convergence <- abs(th.vec[i+1]-th.vec[i]) < delta

    i <- i + 1
  }

  return(list(
    t     = seq(1, i),
    theta = th.vec,
    ell   = LL,
    ellp  = LLp,
    ellp2 = LLp2,
    fun   = fun,
    data  = data,
    likelihood = likelihood
  ))
}

# Wrapper for obtaining the iteration history
get_iter_history <- function(input){

  likelihood <- isolate(t_likelihood(input))
  data       <- isolate(t_data(input))
  start_pos  <- isolate(t_start_pos(input))
  criterion  <- isolate(t_criterion(input))

  return(list(
    "newtraph" = alg_newtraph(likelihood, data, start_pos, criterion),
    "accent"   = alg_accent(likelihood, data, start_pos, criterion)
  ))
}

#### Plotting functions ####
plot_accent   <- function(history, i){

  th.vec <- history$theta
  LL     <- history$ell
  LLp    <- history$ellp
  fun    <- history$fun
  data   <- history$data
  likelihood <- history$likelihood

  if(i > length(th.vec)){
    i         <- length(th.vec)
    converged <- TRUE
  }else{
    converged <- FALSE
  }

  if(likelihood == "pois"){
    x <- seq(1, 5, length.out = 100)
    x_adj <- .25
  }else if(likelihood == "binom"){
    x <- seq(.05, .95, length.out = 100)
    x_adj <- .05
  }

  args       <- data
  args$theta <- x
  out    <- do.call(fun, args)
  loglik <- out$ll
  grad   <- out$grad

  if(likelihood == "pois"){
    x_range <- range(x)
    y_range <- range(loglik)
  }else if(likelihood == "binom"){
    x_range <- c(0, 1)
    y_range <- range(loglik)
  }

  par(mfrow = c(2, 1))

  # general plot
  plot(NA, type = "n", xlim = x_range, ylim = y_range, las = 1,
       main = "The Steepest-Ascent-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))
  lines(x, loglik, lwd = 2)
  abline(h = max(loglik), lty = 2)
  text(x_range[1], y_range[1], paste("Iter: ", i, if(converged) "(Converged)"), adj = c(0, 0), cex = 1.25)
  text(x_range[2], y_range[1], bquote(hat(theta)*"="*.(format(round(th.vec[i], 3), nsmall = 3))), adj = c(1, 0), cex = 1.25)
  if(likelihood == "pois"){
    abline(v = data$mean, lty = 2, lwd = 2, col = "green")
  }else if(likelihood == "binom"){
    abline(v = data$successes / data$n, lty = 2, lwd = 2, col = "green")
  }
  legend(
    "topright",
    legend = c("f(x)", "f'(x)"),
    col    = c("black", "red"),
    lwd    = 2,
    bty    = "n"
  )

  # prediction of the next movement
  if(i + 1 <= length(th.vec)){
    arr_adj <- if(th.vec[i] > th.vec[i+1]) .01 else -.01
    points(th.vec[i + 1], LL[i + 1], col = "grey", pch = 1, lwd = 3)
    arrows(th.vec[i] + arr_adj/2, y_range[1], th.vec[i+1] - arr_adj/2, length = .10)
  }

  # iterations
  points(th.vec[i], LL[i], col = "blue", pch = 20, lwd = 3)
  segments(
    x0 = th.vec[i] -x_adj,
    y0 = (th.vec[i] - x_adj) * LLp[i] + LL[i] - LLp[i] * th.vec[i],
    x1 = th.vec[i] + x_adj,
    y1 = (th.vec[i] + x_adj) * LLp[i] + LL[i] - LLp[i] * th.vec[i],
    col = "red", lwd = 2)

  # Plot the iteration history:
  x_range2 <- if(i == 1) c(1, 2) else c(1,i)
  plot(1:i, th.vec[1:i], xlim = x_range2, ylim = x_range, lwd = 2, type = "b", las = 1,
       main = "Iteration", ylab = "Parameter Estimate", xlab = "")
  if(likelihood == "pois"){
    abline(h = data$mean, lty = 2, lwd = 2, col = "green")
    text(x_range2[1], data$mean, expression(theta), adj = c(0, 0), cex = 1.25)
  }else if(likelihood == "binom"){
    abline(h = data$successes / data$n, lty = 2, lwd = 2, col = "green")
    text(x_range2[1], data$successes / data$n, expression(theta), adj = c(0, 0), cex = 1.25)
  }
}

plot_newtraph <- function(history, i){

  th.vec <- history$theta
  LL     <- history$ell
  LLp    <- history$ellp
  LLp2   <- history$ellp2
  fun    <- history$fun
  data   <- history$data
  likelihood <- history$likelihood

  if(i > length(th.vec)){
    i         <- length(th.vec)
    converged <- TRUE
  }else{
    converged <- FALSE
  }

  if(likelihood == "pois"){
    x <- seq(1, 5, length.out = 100)
    x_adj <- .25
  }else if(likelihood == "binom"){
    x <- seq(.05, .95, length.out = 100)
    x_adj <- .05
  }

  args       <- data
  args$theta <- x
  out    <- do.call(fun, args)
  loglik <- out$ll
  grad   <- out$grad


  if(likelihood == "pois"){
    x_range <- range(x)
    y_range <- range(c(loglik, grad))
  }else if(likelihood == "binom"){
    x_range <- c(0, 1)
    y_range <- range(c(loglik, grad))
  }

  par(mfrow = c(2, 1))

  # general plot
  plot(NA, type = "n", xlim = x_range, ylim = y_range, las = 1,
       main = "The Newton-Raphson-Algorithm", ylab = "Log Likelihood", xlab = expression(theta))
  lines(x, loglik, lwd = 2)
  lines(x, grad,   col = "red")
  abline(h = 0,    lty = 2)
  text(x_range[1], y_range[1], paste("Iter: ", i, if(converged) "(Converged)"), adj = c(0, 0), cex = 1.25)
  text(x_range[2], y_range[1], bquote(hat(theta)*"="*.(format(round(th.vec[i], 3), nsmall = 3))), adj = c(1, 0), cex = 1.25)
  legend(
    "topright",
    legend = c("f(x)", "f'(x)", "f''(x)"),
    col    = c("black", "red", "green"),
    lwd    = 2,
    bty    = "n"
  )
  if(likelihood == "pois"){
    abline(v = data$mean, lty = 2, lwd = 2, col = "green")
  }else if(likelihood == "binom"){
    abline(v = data$successes / data$n, lty = 2, lwd = 2, col = "green")
  }

  # prediction of the next movement
  if(i + 1 <= length(th.vec)){
    arr_adj <- if(th.vec[i] > th.vec[i+1]) .01 else -.01
    points(th.vec[i + 1], LLp[i + 1], col = "grey", pch = 1, lwd = 3)
    arrows(th.vec[i] + arr_adj/2, y_range[1], th.vec[i+1] - arr_adj/2, length = .10)
  }

  # iterations
  points(th.vec[i], LLp[i], col = "blue", pch = 20, lwd = 5)
  lines(c(th.vec[i] - x_adj, th.vec[i] + x_adj), (LLp[i]-(LLp2[i]*th.vec[i])) + LLp2[i]*c(th.vec[i] - x_adj, th.vec[i] + x_adj), col = "green", lwd = 2)

  # Plot the iteration history:
  x_range2 <- if(i == 1) c(1, 2) else c(1,i)
  plot(1:i, th.vec[1:i], xlim = x_range2, ylim = x_range, lwd = 2, type = "b", las = 1,
       main = "Iteration", ylab = "Parameter Estimate", xlab = "")
  if(likelihood == "pois"){
    abline(h = data$mean, lty = 2, lwd = 2, col = "green")
    text(x_range2[1], data$mean, expression(theta), adj = c(0, 0), cex = 1.25)
  }else if(likelihood == "binom"){
    abline(h = data$successes / data$n, lty = 2, lwd = 2, col = "green")
    text(x_range2[1], data$successes / data$n, expression(theta), adj = c(0, 0), cex = 1.25)
  }
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
