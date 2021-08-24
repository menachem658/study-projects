# Project 1
# Name: menachem_sokolik 
# I.D. Number:

# Q1: Replace "return(NA)" by your code
ex1q1 <- function(n, prob){
  x <- numeric(n)  
  for (i in seq(n)) {
    condition <- FALSE
    while (!condition) {
      x[i] <- x[i] + 1
      condition <- runif(1)<prob
    }
  }
  return(x)
}


# Q2: Replace "return(NA)" by your code
ex1q2 <- function(n, k, th){
  z<-(1-(1+th)^(-k))
  x<-((1-runif(n)*z)^(-1/k)-1)
  return(x)
}