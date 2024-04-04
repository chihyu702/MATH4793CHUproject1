#' theoryUnivNormality
#'
#' @param x the num vector going to test univariate Normality through theory
#'
#' @return the the list of vector proportion that within first and second interval
#'          and if it meet the theoretical expectation
#' @export
#'
#' @examples theoryUnivNormality(x = rnorm(100, mean = 0, sd = 1))
theoryUnivNormality <- function(x = rnorm(100, mean = 0, sd = 1)){
  n <- length(x)
  meanX <- mean(x)
  sd <- sd(x)

  # calculate the proportion
  # within 1sd
  p1 <-  sum(x >= meanX - sd & x <= meanX + sd) / n

  # within 2sd
  p2 <- sum(x >= meanX - 2*sd & x <= meanX + 2*sd) / n

  # Expected proportions under normal distribution
  expected1 <- 0.683
  expected2 <- 0.954

  # test value
  test1 <- 1.396/sqrt(n)
  test2 <- 0.628/sqrt(n)

  # nomality test, should show if true or false (meet the theoretical expectation or not)
  test1sdPassed <- abs(p1 - expected1) > test1
  test2sdPassed <- abs(p2 - expected2) > test2

  hist(x, breaks = 10, main = "Histogram of x")

  # print out the result
  return(list(
    p1_hat = p1,
    p2_hat = p2,
    test1sdPassed = test1sdPassed,
    test2sdPassed = test2sdPassed
  ))

}
