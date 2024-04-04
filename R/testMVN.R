#' testMVN
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
testMVN <- function(x, dim=2){
  # x is a matrix v
  n <-nrow(x)
  meanX <- colMeans(x)
  S <- cov(x)
  S_inv <- solve(S) # inverse matrix of S

  # number of distance store in d
  # d is the vector of distance(square)
  d <- numeric(n)
  # calculate distance from center
  # result 4.7
  for (i in 1:n){
    d[i]=t(x[i,]-meanX)%*%S_inv%*%(x[i,]-meanX)
  }
  d <- sort(d)

  # number of distance store in d
  # q is the vector of quantile
  q <- numeric(n)
  for (i in 1:n){
    q[i]= qchisq(( i - 0.5) / n, df=dim)
  }

  plot(q,d, xlab=paste("q(i-0.50)/", n), ylab="di^2")

  # plot the chi-square plot
  return(list(d=round(d,4), q=round(q,4)))
}
