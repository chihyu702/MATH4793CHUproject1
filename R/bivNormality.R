#' bivNormality
#'
#' @param x the data set contain two variables
#' @param alpha the percentage that we expected to
#'
#' @return an ellipse around the bivariate data and a list containing
#' the actual and predicted percentage contents of the ellipse

#' @export
#'
#' @examples bivNormality(x=matrix(c(T4_6$V1, T4_6$V2), byrow=FALSE))
bivNormality <- function(x, alpha=0.08){
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

  # d<=chi-square distribution with p degrees of freedom.
  # variables = 2, df = 2
  chi_test<-qchisq(1-alpha, df =2)
  # number of data that are within the ellipse (satisfy the equation,d<=chi-square)
  inside <- d<=chi_test
  count <- sum(d<=chi_test)
  # expected percentage
  actualper <- count/n

  # expected percentage
  expectedper <- 1-alpha

  # invisible(plot(x))
  # return the list
  # sine the d is actually d^2, so sqrt it here to make it become the actual distance
  list(
    ellipse = round(d,4),
    within_the_contour = inside,
    actual_percentage = actualper,
    expected_percentage =  expectedper
  )
}
