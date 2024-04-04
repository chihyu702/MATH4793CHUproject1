#' boxcox
#'
#' carries out the boxcox transformation
#'
#' @param xs  the vector
#' @param alpha the ci
#' @param iter number of time we are going to run
#'
#' @return a list
#' @export
#'
#' @importFrom stats optimize
#'
#' @examples
boxcox <- function(xs, alpha=0.05, iter=1000){
  n <- length(xs)
  xl <- function(lambda, x) {
    if(lambda != 0)
    {
      (x^lambda-1)/lambda
    }
    else(
      log(x)
    )
  }

  lamb <- function(l, x){
    xx <- xl(lambda = l, x = x)
    -n/2*log(1/n*sum((xx - mean(xx))^2)) + (l-1)*sum(log(x))
  }
  lmax <- vector(mode = "numeric", length = iter)
  for(i in 1:iter){

    xbrs <- sample(x = xs,size = n,replace = i != 1)
    lmax[i] <- optimize(f = lamb,interval = c(-5,5), x = xbrs, maximum = TRUE)$maximum
  }

  hist(lmax)
  abline(v = lmax[1], lwd = 2, col = "Blue")
  ci <- quantile(lmax, c(alpha/2, 1-alpha/2))
  segments(ci[1],0,ci[2],0,col = "Red",lty = 2,lwd = 3)
  invisible(list(lambda = lmax, ci = ci))
}
