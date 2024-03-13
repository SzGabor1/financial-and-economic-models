CalcA2 <- function(X,Y){


#top
sumx2y <- Summary(X^2,Y^1) 
sumx3 <- Summary(X^3)
sumx2 <- Summary(X^2) 
sumxy <- Summary(X^1,Y^1) 
sumx2 <- Summary(X^2) 
sumx <- Summary(X^1) 
sumy <- Summary(Y^1) 
n = length(x)

#bottom
sumx4 <-Summary(X^4)



#determinant
#upper

det_upper = sumx2y*(sumx2*n - sumx * sumx) - sumx3*(sumxy*n - sumy*sumx) + sumx2*(sumxy*sumx - sumy*sumx2)
print(det_upper)
det_bottom = sumx4*(sumx2*n-sumx*sumx) - sumx3*(sumx3*n - sumx2*sumx) + sumx2*(sumx3*sumx - sumx2*sumx2)
print(det_bottom)
#divide 2dets
A2= det_upper/det_bottom
print(A2)
}

Summary <- function(X, Y) {
  sum <- 0
  
  if (missing(Y)) {
    for (i in seq_along(X)) {
      sum <- sum + X[i]
    }
    return(sum)
  }
  
  for (i in seq_along(X)) {
    sum <- sum + X[i] * Y[i]
  }
  return(sum)
}

X = rnorm(300);
Y = rnorm(300);
CalcA2(X,Y);