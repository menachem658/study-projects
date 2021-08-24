# Project 4
# Name: Menachem Sokolik
# I.D. Number: 


# Q1: Replace "return(NA)" bY Your code
ex4q1 <- function(n_sim, n_conv, a, b){X_sim <- Y_sim <- rep(NA, n_sim)
  f1 <- rnorm(1, mean = b[1]/a[1], sd = sqrt(1/a[1]))
  for (i in seq(n_sim)) {X_sim[i] <- f1
    Y_sim[i] <- rnorm(1, mean = b[2]/(f1^2+a[2]), sd = 1/sqrt(f1^2+a[2]))
    f1 <- rnorm(1, mean = b[1]/(Y_sim[i]^2+a[1]), sd = 1/sqrt(Y_sim[i]^2+a[1]))}
  res <- cbind(X_sim,Y_sim)
  return(res)}


# Q2: Replace "return(NA)" bY Your code
ex4q2 <- function(data, mu, tau, r, lam, n_sim, n_conv){x <- 0
  f1 <- rgamma(1,r+length(data)/2,lam+(sum(data^2)*0.5))
  for(i in seq(n_conv)){
    x <- rnorm(1,mean=(mu/tau^2+sum(data)*f1)/(1/tau^2+length(data)*f1),sd=sqrt(1/(1/tau^2+length(data)*f1)))
    f1 <- rgamma(1,r+length(data)/2,lam+0.5*sum((data-a)^2))}
  y<- rep(NA,n_sim)
  f2 <- rep(NA,n_sim)
  x <- rnorm(1,mean=(mu/tau^2+sum(data)*f1)/(1/tau^2+length(data)*f1),sd=sqrt(1/(1/tau^2+length(data)*f1)))
  for(i in seq(n_sim)){y[i] <- x
    f2[i] <- rgamma(1,r+length(data)/2,lam+0.5*sum((data-x)^2))
    x <- rnorm(1,mean=(mu/tau^2+f2[i]*sum(data))/(1/tau^2+f2[i]*length(data)),sd=sqrt(1/(1/tau^2+f2[i]*length(data))))}
  return(as.matrix(data.frame(x=y,f1=f2)))}





