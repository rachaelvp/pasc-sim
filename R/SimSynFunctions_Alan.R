## Functions called for synthetic data simulations

## Generate data from known DGD

generate_data_simple <- function(N,Xranges=c(-1,1,-1,1),betaA=c(0,0.1,-0.4),
                                 betaY0=c(0,1,2,-1),betaC=c(1,7/5,5,3),sdy=1){
  # A simple data generating process
  X1 <- runif(N,Xranges[1],Xranges[2])
  X2 <- runif(N,Xranges[3],Xranges[4])
  pi0 <- plogis(betaA[1]+betaA[2]*X1*X2+betaA[3]*X1)
  A <- rbinom(N,1,prob=pi0)
  muY0 <- betaY0[1]+betaY0[2]*X1*X2 + betaY0[3]*X2^2 +betaY0[4]*X1
  CATE <- betaC[1]*X1^2*(X1+betaC[2]) + (betaC[3]*X2/betaC[4])^2
  muY = muY0+A*CATE
  Y <- rnorm(N,sd=sdy,mean= muY)
  return(tibble(X1=X1,X2=X2,A=A,Y=Y))
}


## Get True_values from known DGD

True_values <- function(N=100000,Xranges=c(-1,1,-1,1),
                        betaA=c(0,0.1,-0.4),                                   betaY0=c(0,1,2,-1),
                        betaC=c(1,7/5,5,3),sdy=1)
{
  # A simple data generating process
  X1 <- runif(N,Xranges[1],Xranges[2])
  X2 <- runif(N,Xranges[3],Xranges[4])
  pi0 <- plogis(betaA[1]+betaA[2]*X1*X2+betaA[3]*X1)
  P0A = mean(pi0)
  A <- rbinom(N,1,prob=pi0)
  muY0 <- betaY0[1]+betaY0[2]*X1*X2 + betaY0[3]*X2^2 +betaY0[4]*X1
  CATE <- betaC[1]*X1^2*(X1+betaC[2]) + (betaC[3]*X2/betaC[4])^2
  true.ATE <- mean(CATE)
  muY = muY0+A*CATE
  Y <- rnorm(N,sd=sdy,mean= muY)
  P0Y <- mean(Y)
  coef.work <- coef(lm(Y~A+X1+X2))
  return(list(P0A=P0A,P0Y=P0Y,true.ATE=true.ATE,coef.work))
}

# Get estimates of simple parameters
get.estimates <- function(df) {
  aveAn <- mean(df$A)
  aveYn <- mean(df$Y)
  lmn <- lm(Y~A+X1+X2,data=df)
  coeffn <- summary(lmn)$coefficients[,1:2]
  return(list(aver = c(aveAn,aveYn),reg = coeffn))
}

