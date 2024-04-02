#' bivNormality
#'
#' @param x the dataset contain two variabes
#'
#' @return
#' @export
#'
#' @examples
bivNormality <- function(x, alpha=0.08){
  # x is a matrix
  n <-nrow(x)
  meanX <- colMeans(x)
  S <- cov(x)
  S_inv <- solve(S) # inverse matrix of S

  # number of distance store in d
  # d is the vector of distance(square)
  d <- numeric(n)

  # calculate distance from center
  for (i in 1:n){
    d[i]=t(x[i,]-meanX)%*%S_inv%*%(x[i,]-meanX)
  }

  # d<=chi-square distribution with p degrees of freedom.
  # variables = 2, df = 2
  chi_test<-qchisq(1-alpha, df =2)
  # number of data that are within the ellipse (satisfy the equation,d<=chi-square)
  count <- sum(d<=chi_test)
  # expected percentage
  actualper <- count/n

  # expected percentage
  expectedper <- 1-alpha

  list(
    ellipse = round(d,4),
    actual_percentage = actualper,
    expected_percentage =  expectedper
  )
}
