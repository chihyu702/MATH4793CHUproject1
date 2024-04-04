#' corQ
#'
#' @param x the vector of the original data
#'
#' @return the vector of correlation coefficient of its Q-Q plot
#' @export
#'
#' @examples
corQ <- function(x) {
  n <- length(x)

  # number of distance store in d
  # q is the vector of quantile
  q <- numeric(n)
  for (i in 1:n){
    q[i] = qchisq((i-0.5)/n, df=2)
  }

  # calculate the rQ, base on 4-31
  meanX <- mean(x)
  meanQ <- mean(q)

  up <- sum((x - meanX) * (q - meanQ))
  denominator <- sqrt(sum((x - meanX)^2) * sum((q - meanQ)^2))

  rQ <- up / denominator
  rQ
}
