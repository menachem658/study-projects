#####################
# Testing Project 3 #
#####################
rm(list=ls())

# RUN THIS CODE IN THE SAME DIRECTORY WHERE YOUR FILE IS LOCATED
# REPLACE THE NAME TO THE NAME OF YOUR FILE

file_name <- "Project_3.R"
####################################
require(stringr)
require(MASS)
require(boot)

pass <- TRUE

# Check that the code does not contain "boot"
script <- paste(scan(file_name, what = "a"), collapse = "")
if (any(str_detect(script, c("boot")))){
  pass <- FALSE
  stop("Your code containes `boot`")
}

source(file_name)
# Q1
# Checks that the code for "ex3q1" produces a matrix 
# of dimension n X m
n <- sample(500:1000,1)
m <- sample(5:10,1)
mu <- rnorm(m)
sig <- runif(1,1,2)
r <- runif(1,-0.9,0.9)
X <- ex3q1(n, mu, sig, r)
if((dim(X)[1] != n) | (dim(X)[2] != m) | (!is.matrix(X))){
  stop("ex3q1 does not produce a matrix of dimension n X m")
}

# Checks the marginal distributions
X <- ex3q1(10^6, mu, sig, r)
test.margin <- rep(FALSE, m)
z <- seq(-2,2,length.out = 10^2)
for (j in seq(m)){
  x <- z*sig + mu[j]
  cdf <- ecdf(X[,j])
  test.margin[j] <- max(abs(cdf(x)-pnorm(x,mu[j],sig))) < 10^(-2)
}
if (!all(test.margin)){
  pass <- FALSE
  stop("A marginal distribution in ex3q1 is wrong")
}

# Checks the distribution of the sum
Y <- rowSums(X)
cdf <- ecdf(Y)
index <- 1:(m-1)
sd_Y <- sig*sqrt(m+2*sum(r^index*(rev(index))))
y <- z*sd_Y + sum(mu)
cdf <- ecdf(Y)
test.sum <- max(abs(cdf(y)-pnorm(y,sum(mu),sd_Y))) < 10^(-2)
if (!test.sum){
  pass <- FALSE
  stop("The distribution of the sum in ex3q1 is wrong")
}

# Q2
# Checks that the code for "ex3q2" produces a list vector
# of length 2 with the two components
n <- 10^2
m <- 3
mu <- rnorm(m)
A <- matrix(rnorm(m^2),m,m)
Sig <- A%*%t(A)
Y <- mvrnorm(n,mu,Sig)
max.mean <- function(Y) max(colSums(Y))
R <- sample(50:100,1)
out <- ex3q2(Y, max.mean, R)
if((length(out) != 2) | (!is.list(out)) | 
   (names(out)[1] != "t0") | (names(out)[2] != "t")){
  pass <- FALSE
  stop("ex3q2 does not produce an output according to the request")
}

# Check that t0 producess the value of the statistic
# and taht t is of the correct length
test.length <- test.val <- rep(FALSE, 2)
test.length[1] <- (length(out$t) == R)
test.val[1] <- (out$t0 == max.mean(Y))
R <- sample(50:100,1)
out <- ex3q2(Y, function(X) max(diag(var(X))), R)
test.length[2] <- (length(out$t) == R)
test.val[2] <- (out$t0 == max(diag(var(Y))))
if (!all(test.length)){
  pass <- FALSE
  stop("The length of `t`` is not equal to R")
}
if (!all(test.val)){
  pass <- FALSE
  stop("The value of `t0`` is not equal to the 
       application of the statistic to the data")
}

# Check the bootstrap districution
test.boot <- rep(FALSE,2)
mle <- list(mu = colMeans(Y), Sig = var(Y))
Y.rg <- function(data, mle) mvrnorm(nrow(data),mle$mu,mle$Sig)
R <- 10^5
# function 1
boot_out <- boot(Y, max.mean, R, sim = "parametric",
                 ran.gen = Y.rg, mle = mle)
out <- ex3q2(Y, max.mean, R)
y_range <- range(boot_out$t, out$t)
y <- seq(y_range[1], y_range[2],length.out = 10^2)
cdf1 <- ecdf(boot_out$t)
cdf2 <- ecdf(out$t)
test.boot[1] <- max(abs(cdf1(y)-cdf2(y))) < 10^(-2)
# function 2
boot_out <- boot(Y, function(X) max(diag(var(X))), R, 
                 sim = "parametric",
                 ran.gen = Y.rg, mle = mle)
out <- ex3q2(Y, function(X) max(diag(var(X))), R)
y_range <- range(boot_out$t, out$t)
y <- seq(y_range[1], y_range[2],length.out = 10^2)
cdf1 <- ecdf(boot_out$t)
cdf2 <- ecdf(out$t)
test.boot[2] <- max(abs(cdf1(y)-cdf2(y))) < 10^(-2)
if (!all(test.boot)){
  pass <- FALSE
  stop("The function ex3q2 producess a wrong bootstrap 
       distribution of the statistic")
}

pass