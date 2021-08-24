# Project 3
# Name: menachem sokolik
# I.D. Number: 


# Q1: Replace "return(NA)" by your code
if (!require("comprehenr")) install.packages("comprehenr")
library(comprehenr)
ex3q1 <- function(n, mu, sig, r){
  sigma <- to_vec(for (x in seq(length(mu))) for (y in seq(length(mu))) (r^abs(x-y))*(sig^2))
  sig_root <- matrix(sigma, length(mu), length(mu))
  sig_eigen<- eigen(sig_root,symmetric=T)
  U <- sig_eigen$vectors
  Lam <- diag(sqrt(sig_eigen$values))
  sig_root_eigen <- U %*% Lam %*% t(U)
  X <- matrix(rnorm(n*length(mu)),length(mu),n)
  y <- sig_root_eigen %*% X
  y <- sweep(y,1,mu,"+")
  return(t(y))
}

# Q2: Replace "return(NA)" by your code
ex3q2 <- function(data, statistic, R){
  t <- rep(NA, R)
  t0 <- statistic(data)
  for (i in c(1:R)){
    mu_obs <- colMeans(data)
    sig_obs <- var(data)
    Y_sim <- mvrnorm(10^2,mu_obs,sig_obs)
    t[i] <-  statistic(Y_sim)
  }
  return(list("t0"=t0, "t"=t))
}

