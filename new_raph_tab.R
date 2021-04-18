### new_raph_tab ----
new_raph_tab <- tabItem(
  tabName = "new_raph_tab",
  plotOutput("new_raph_plot")
)

# server <- function(input, output)
# {
# ###########iteration process
# #input data
# dat <- c(2,2,3,4,4)
# n <- length(dat)
# 
# #steepest ascent algorithm
# 
# maxiter=500; alpha=.015; delta=10^-6
# th.vec=NULL; LL=NULL; LLp=NULL
# th.vec[1]=2
# 
# out<-loglikpois(th.vec[1])
# LL[1]=out$ll; LLp[1]=out$grad
# convergence=FALSE
# a=1
# while ((!convergence)&(a<maxiter))
# {
#   th.vec[a+1] <- th.vec[a] + alpha*out$grad
#   out<-loglikpois(th.vec[a+1])
#   LL[a+1]=out$ll; LLp[a+1]=out$grad
#   if ((abs(th.vec[a+1]-th.vec[a]))<delta) {convergence=TRUE}
#   a <- a+1
# }
# 
# iterhist = data.frame(t=seq(1,a),theta=th.vec,ell=LL,ellp=LLp)
# #iterhist could be displayed
# }