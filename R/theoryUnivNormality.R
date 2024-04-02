#' theoryUnivNormality
#'
#' @param x the num vector going to test univariate Normality through theory
#'
#' @return the the list of vector proportion that within first and second interval
#'          and if it meet the theoretical expectation
#' @export
#'
#' @examples theoryUnivNormality(x = rnorm(100, mean = 0, sd = 1))
theoryUnivNormality <- function(x){
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)

  # calculate the proportion
  # within 1sd
  p1 <-  sum(x >= mean - sd & x <= mean + sd) / n

  # within 2sd
  p2 <- sum(x >= mean - 2*sd & x <= mean + 2*sd) / n

  # Expected proportions under normal distribution
  expected1 <- 0.683
  expected2 <- 0.954

  # test value
  test1 <- 1.396/sqrt(n)
  test2 <- 0.628/sqrt(n)

  # nomality test, should show if true or false (meet the theoretical expectation or not)
  test1sd <- abs(p1-expected1) > test1
  test2sd <- abs(p2-expected2) > test2

  # print out the result
  return(list(
    p1_hat = p1,
    p2_hat = p2,
    propWithinFirstsd= test1sd,
    propWithinSecondsd= test1sd
  ))
}
