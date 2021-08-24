#####################
# Testing Project 2 #
#####################
rm(list=ls())

# RUN THIS CODE IN THE SAME DIRECTORY WHERE YOUR FILE IS LOCATED
# REPLACE THE NAME TO THE NAME OF YOUR FILE
# FOR THIS TEST YOU NEED TO INSTALL "cubature"

file_name <- "Project_2.R" 

####################################
require(cubature)
require(stringr)

pass <- TRUE

# Check that the code does not contain "cubature"
script <- paste(scan(file_name, what = "a"), collapse = "")
if (any(str_detect(script, c("cubature")))){
  pass <- FALSE
  stop("Your code containes `cubature`")
}

source(file_name)
# Q1
# Checks that the code for "ex2q1" produces a data frame 
# of dimension n X 2
n <- sample(500:1000,1)
X <- ex2q1(n)
if((dim(X)[1] != n) | (dim(X)[2] != 2) | (!is.data.frame(X))){
  stop("ex2q1 does not produce a data frame of dimension n X 2")
}

# Checks the marginal distribution
XY <- ex2q1(10^6)
f1 <- function(x) (6/pi)*sqrt((1-sum(x^2))*(sum(x^2) < 1))
test.margin <- rep(FALSE, 10)
for(i in seq(test.margin)){
  x <- runif(2)
  P1 <- mean((XY[,1] <= x[1]) &  (XY[,2] <= x[2]))
  P2 <- hcubature(f1, lower = rep(0,2), upper = x)$integral
  test.margin[i] <- abs(P1 - P2) < 0.01
}
if (!all(test.margin)){
  pass <- FALSE
  stop("The marginal distribution in ex2q1 is wrong")
}


# Q2
# Checks that the code for "ex2q2" produces a list vector
# of length n
k <- sample(3:6,1)
alpha <- runif(k)
n.copy <- sample(500:1000,1)
I <- ex2q2(alpha,n.copy)
if((length(I) != 2) | (!is.list(I)) | (names(I)[1] != "value") | (names(I)[2] != "error")){
  pass <- FALSE
  stop("ex2q2 does not produce an output according to the request")
}

# Checks the integral
test.integral <- rep(FALSE, 10)
n.copy <- 10^6
for(i in seq(test.integral)){
  k <- sample(2:5,1)
  alpha <- runif(k)
  f1 <- function(x) sin(sum(x*alpha))
  I1 <- ex2q2(alpha, n.copy)$value
  I2 <- hcubature(f1, lower = rep(0,k), upper = rep(1,k))$integral
  test.integral[i] <- abs(I1 - I2) < 0.01
}
if (!all(test.integral)){
  pass <- FALSE
  stop("The integral computed by ex2q2 is wrong")
}

pass
