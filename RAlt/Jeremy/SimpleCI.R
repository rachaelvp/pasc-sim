mu<- 10
sgma <- 1
n <- 100
X <- rnorm(n,mu,sgma) 

xbar <- mean(X)
sigman <- sd(X)
B <- 10000
out <- NULL
for(i in 1:B){
  Xstar <- rnorm(n,xbar,sigman)
  Xsbar <- mean(Xstar)
#  sigmaXs <- sd(Xstar)
sigmaXs <- sgma
    up <- Xsbar + 1.96*sqrt(2)*sigmaXs/sqrt(n)
  low <- Xsbar - 1.96*sqrt(2)*sigmaXs/sqrt(n)
  out <- rbind(out,c(Xsbar,low,up))
}

cover <- function(x,mu){
  mu < x[1] | mu > x[2]
}

c.res <- apply(out[,2:3],1,cover,mu=mu)
table(c.res)