#####################
# Testing Project 4 #
#####################
rm(list=ls())

# RUN THIS CODE IN THE SAME DIRECTORY WHERE YOUR FILE IS LOCATED
# REPLACE THE NAME TO THE NAME OF YOUR FILE

file_name <- "Project_4.R"
#"C:\Users\menac\OneDrive\Desktop\52324 Probability for Statistics students\project4\314696972_Project_4.R"
####################################
pass <- TRUE

source(file_name)

# Q1
# Checks that the code for "ex4q1" produces a matrix 
# of dimension n_sim X 2
n_sim <- sample(500:1000,1)
n_conv <- sample(50:100,1)
a <- runif(2) + 0.5
b <- rnorm(2)
XY <- ex4q1(n_sim, n_conv, a, b)
if((!is.matrix(XY)) | (dim(XY)[1] != n_sim) | (dim(XY)[2] != 2)){
  pass <- FALSE
  stop("ex2q1 does not produce a matrix of dimension n_sim X 2")
}

n <- 10^6
eps <- 0.01
XY <- ex4q1(n, 10^3, a, b)

# Checks the algorithm
X.then.Y <- FALSE
ZY <- (XY[,2] - b[2]/(a[2] + XY[,1]^2))*sqrt(a[2]+XY[,1]^2)
ZX <- (XY[-1,1] - b[1]/(a[1] + XY[-n,2]^2))*sqrt(a[1]+XY[-n,2]^2)
moments <- c(mean(ZY), mean(ZX), sd(ZY) - 1, sd(ZX) -1)
X.then.Y <- max(abs(moments)) < eps
Y.then.X <- FALSE
ZY <- (XY[-1,2] - b[2]/(a[2] + XY[-n,1]^2))*sqrt(a[2]+XY[-n,1]^2)
ZX <- (XY[,1] - b[1]/(a[1] + XY[,2]^2))*sqrt(a[1]+XY[,2]^2)
moments <- c(mean(ZY), mean(ZX), sd(ZY) - 1, sd(ZX) -1)
Y.then.X <- max(abs(moments)) < eps
if (!((X.then.Y & !Y.then.X) | (Y.then.X & !X.then.Y))){
  pass <- FALSE
  stop("The Gibbs algorithm is not applied correctly in ex4q1")
}


# Check the distribution of the output
X1 <- rnorm(n*10, b[1]/a[1], 1/sqrt(a[1]))
Y1 <- rnorm(n*10, b[2]/a[2], 1/sqrt(a[2]))
U1 <- runif(n*10)
accept <- -log(U1) > 0.5*X1^2*Y1^2
X1 <- X1[accept]
Y1 <- Y1[accept]
x <- runif(10,0,3)
y <- runif(10,0,3)
P_Gibbs <-  rep(10,0)
P_Reject <- P_Gibbs + 1 
for (i in 1:10){
  P_Gibbs[i] <- mean((XY[,1] < x[i]) & (XY[,2] < y[i]))
  P_Reject[i] <- mean((X1 < x[i]) & (Y1 < y[i]))
}
if (!all(max(abs(P_Gibbs - P_Reject)) < eps)){
  pass <- FALSE
  stop("The distribution produced by ex4q1 is not correct")
}  


# Q2
# Checks that the code for "ex4q2" produces a matrix 
# of dimension n_sim X m
n_sim <- sample(500:1000,1)
n_conv <- sample(50:100,1)
n_data <- sample(20:30,1)
mu <- rnorm(1)
tau <- runif(1,1,3)
r <- runif(1,3,7)
lam <- runif(1) + 0.5
Y <- rgamma(1,r,lam)
M <- rnorm(1,mu, tau)
data <- rnorm(n_data,M,1/sqrt(Y))
MY <- ex4q2(data, mu, tau, r, lam, n_sim, n_conv)
if((!is.matrix(MY)) | (dim(MY)[1] != n_sim) | (dim(MY)[2] != 2)){
  pass <- FALSE
  stop("ex2q2 does not produce a matrix of dimension n_sim X 2")
}

# Check marginal distributions
n <- 10^6
eps <- 0.01

# M
lam <- r <- 10^6
n_data <- sample(7:15,1)
data <- rnorm(n_data)
mu <- rnorm(1)
tau <- runif(1,1,3)
MY <- ex4q2(data, mu, tau, r, lam, n, 10^3)
sig1 <- 1/sqrt(1/tau^2+n_data)
m1 <- (mu/tau^2 + sum(data))*sig1^2
ZM <- (MY[,1]-m1)/sig1
CDF.M <- ecdf(ZM)
z <- seq(-2,2,length.out = 10)
M.is.Normal <- all(abs(CDF.M(z)-pnorm(z)) < eps)

# Y
tau <- 1/10^6
mu <- 0
n_data <- sample(7:15,1)
data <- rnorm(n_data)
r <- runif(1,3,7)
lam <- runif(1) + 0.5
MY <- ex4q2(data, mu, tau, r, lam, n, 10^3)
CDF.Y <- ecdf(MY[,2]*(lam + 0.5*sum(data^2)))
z <- seq(0,2*sqrt(r+n_data/2),length.out = 10)
Y.is.Gamma <- all(abs(CDF.Y(z)-pgamma(z,r+n_data/2,1)) < eps)

if((!M.is.Normal) | (!Y.is.Gamma)){
  pass <- FALSE
  stop("ex2q2 does not produce correct marginal distributions")
}

pass
