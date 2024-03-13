CalcA2 <- function(X,Y){


	#top
	sumx2y <- Summary(X^2,Y^1) 
	sumx3 <- Summary(X^3)
	sumx2 <- Summary(X^2) 
	sumxy <- Summary(X^1,Y^1) 
	sumx2 <- Summary(X^2) 
	sumx <- Summary(X^1) 
	sumy <- Summary(Y^1) 
	n = length(X)

	#bottom
	sumx4 <-Summary(X^4)



	#determinants
	#upper

	det_upper = sumx2y*(sumx2*n - sumx * sumx) - sumx3*(sumxy*n - sumy*sumx) + sumx2*(sumxy*sumx - sumy*sumx2)
	#print(det_upper)
	det_bottom = sumx4*(sumx2*n-sumx*sumx) - sumx3*(sumx3*n - sumx2*sumx) + sumx2*(sumx3*sumx - sumx2*sumx2)
	#print(det_bottom)
	#divide 2dets
	A2= det_upper/det_bottom
	print(A2)
}


CalcA1 <- function(X,Y){
 
#A1 top-bottom
sumx4 <- Summary(X^4)
sumx2y<-Summary(X^2,Y)
sumx2<- Summary(X^2)
sumx3<- Summary(X^3)
sumxy <-Summary(X,Y)
sumx<- Summary(X)

sumy<-Summary(Y)

n = length(X)



	det_top = sumx4*(sumxy*n-sumy*sumx) - sumx2y *(sumx3*n-sumx2*sumx) + sumx2*(sumx3*sumy-sumx2*sumxy)
	#print(det_top)
	det_bottom = sumx4*(sumx2*n-sumx*sumx) - sumx3*(sumx3*n-sumx2*sumx) + sumx2*(sumx3*sumx-sumx2*sumx2)
	#print(det_bottom)
	
	A1=det_top/det_bottom
	print(A1)
	


}

CalcA0 <- function(X, Y) {
  n <- length(X)
  sumx4 <- Summary(X^4)
  sumx3 <- Summary(X^4)
  sumx2y <- Summary(X^2, Y)
  sumx3 <- Summary(X^3)
  sumx2 <- Summary(X^2)
  sumxy <- Summary(X,Y)
  sumx <- Summary(X)
  sumy <- Summary(Y)

  upper <- sumx4 * (sumx2 * sumy - sumx * sumxy) - sumx3 * (sumx3 * sumy - sumx2 * sumxy) + sumx2y * (sumx3 * sumx - sumx2 * sumx2)
	#print(upper)  
	bottom <- sumx4 * (sumx2 * n - sumx * sumx) - sumx3 * (sumx3 * n - sumx2 * sumx) + sumx2 * (sumx3 * sumx - sumx2 * sumx2)
#print(bottom)
  A0 <- upper / bottom
  print(A0)
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

Polreg <- function(){
X=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

Y <- 5 * X^2 + 3 * X + 7
#Y <- 3*X+7
A2 = CalcA2(X,Y);
A1 = CalcA1(X,Y);
A0 = CalcA0(X,Y);

plot(X,Y)

lines(X,A2*X^2+A1*X+A0, type="l", lty = 1, col = "red")
}


Polreg()

