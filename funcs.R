#Log likelihood algorithm
loglikpois <- function(dat, theta, n){
  ll <- -n*theta+sum(dat)*log(theta)
  grad <- -n+sum(dat)/theta
  hess <- -sum(dat)/theta^2
  out<-list(ll=ll,grad=grad,hess=hess)
  return(out)
}