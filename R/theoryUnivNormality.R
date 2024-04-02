#' theoryUnivNormality
#'
#' @param x the dataset going to test univariate Normality through theory
#'
#' @return
#' @export
#'
#' @examples
theoryUnivNormality <- function(x){
  n <- length(x)
  mean <- mean(x)
  sd <- sd(x)

  # calculate the observed proportion
  # within 1sd
  p1 <- ((x >= mean - sd) + (x <= mean + sd))/n
  cat("p1 hat = ", p1)
  # within 2sd
  p2 <- ((x >= mean - 2*sd) + (x <= mean+2*sd))/n
  cat("p2 hat = ", p2)


  # Expected proportions under normal distribution
  expected1 <- 0.683
  expected2 <- 0.954

  # test value
  test1 <- 1.396/sqrt(n)
  test2 <- 0.628/sqrt(n)

  # nomality test
  test1sd <- abs(p1-expected1) > test1
  test2sd <- abs(p2-expected2) > test2

  cat("test first interval is:", test1sd)
  cat("test second interval is:", test2sd)

  return(list(
    pl_hat = p1,
    p2_hat = p2,
    testFirstInterval = test1sd,
    testSecondInterval = test2sd,
  )

  )
}
