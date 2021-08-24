# Project 2
# Name: menachem sokolik
# I.D. Number:

# Q1: Replace "return(NA)" by your code
ex2q1 <- function(n){
  XY <- matrix(NA,n,2)
  colnames(XY) <- c("X","Y") 
  for (i in seq(n)){
    accept <- FALSE
    while (!accept) {
      attempt <- runif(2,0,1)
      U <- runif(1)
      if(sum(attempt^2)<1){
        accept <- (U <= (sqrt(1-sum(attempt^2))))
      }
    }
    XY[i,] <- attempt
  }
  return(data.frame(XY))
}


# Q2: Replace "return(NA)" by your code
ex2q2 <- function(alpha, n.copy){
  x = sapply(1:n.copy, function(index){
    den <- runif(length(alpha)) 
    y <- mean(sin(sum(den*alpha)))})
  error=sd(x)/length(alpha)
  return(list("value"=mean(x),"error"= error))
}

  