#Log likelihood algorithm
loglikpois <- function(dat, theta, n){
  ll <- -n*theta+sum(dat)*log(theta)
  grad <- -n+sum(dat)/theta
  hess <- -sum(dat)/theta^2
  out<-list(ll=ll,grad=grad,hess=hess)
  return(out)
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

