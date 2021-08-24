#####################
# Testing Project 1 #
#####################
rm(list=ls())

# RUN THIS CODE IN THE SAME DIRECTORY WHERE YOUR FILE IS LOCATED
# REPLACE THE NAME TO THE NAME OF YOUR FILE

file_name <- "C:\\Users\\menac\\OneDrive\\Desktop\\Q\\Project_1.R"

####################################

pass <- TRUE

# Check that the code does not contain "rgeom" nor "rexp"
require(stringr)
script <- paste(scan(file_name, what = "a"), collapse = "")
if (any(str_detect(script, c("rgeom", "rexp")))){
  pass <- FALSE
  stop("Your code containes `rgeom` or `rexp` ")
}

source(file_name)
# Q1
# Checks that the code for "ex1q1" produces a numeric vector 
# of length n
n <- sample(500:1000,1)
prob <- runif(1,0.25,0.75)
X <- ex1q1(n,prob)
if((length(X) != n) | (!is.numeric(X))){
  stop("ex1q2 does not produce a numeric vector of length n")
}

# Checks the marginal distribution
n <- 10^6
d <- max(abs(table(ex1q1(n,prob))[1:10]/n - dgeom((1:10)-1,prob)))
if (d > 0.01){
  pass <- FALSE
  stop("The marginal distribution in ex1q1 is not geometric")
} 

# Q2
# Checks that the code for "ex1q2" produces a numeric vector 
# of length n
n <- sample(500:1000,1)
k <- runif(1, 2, 5)
th <- runif(1,2,5)
X <- ex1q2(n,k,th)
if((length(X) != n) | (!is.numeric(X))){
  pass <- FALSE
  stop("ex1q2 does not produce a numeric vector of length n")
}

# Checks the marginal distribution
n <- 10^6
k <- runif(1, 2, 5)
th <- runif(1,2,5)
X <- ex1q2(n,k,th)
cdf <- ecdf(X)
my.cdf <- function(x, k, th){
  term1 <- 1-(1+th)^(-k)
  return((1-(1+x)^(-k))/term1)
}
x <- seq(0,th, length.out = 10)
d <- max(abs(cdf(x)-my.cdf(x,k,th)))
if (d > 0.001){
  pass <- FALSE
  stop("The marginal distribution in ex1q2 is wrong")
} 

pass
