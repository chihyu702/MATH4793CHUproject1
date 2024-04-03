#' DetectingOutliers
#'
#' this is the function that would carries out step 1-4 "Setps for Detecting outliers" in textbokk
#'
#' @param x the data set
#'
#' @return the matrix of s, the generalized squared distance, and the outlier
#' @export
#'
#' @examples DetectingOutliers (T1_4)
DetectingOutliers <- function(x){
  n <- nrow(x)
  p <- ncol(x)
  S <- cov(x)
  means <- colMeans(x) #col mean
  S_inv <- solve(S) # inverse matrix of S

  # Make a dot plot for each variable.
  for (i in 1:p){
    stripchart(x[,1], method = "stack", main = "Dot Plot for Variable", xlab = paste("Variable", i), pch = 19)
  }

  # Make a scatter plot for each pair of variables.
  for (i in 1:(p-1)){
    for (j in (i+1):p){
      plot(x[,i], x[,j],  xlab = paste("Variable", i), ylab = paste("Variable", j))
    }
  }

  # calculate the standardized values, z.
  z <- matrix(nrow=n, ncol=p)
  for (j in 1:n){
    for (k in 1:p){
      z[j,k] = (x[j,k]- means[k]/sqrt(S[k,k]))
    }
  }

  # calculate the generalized squared distance
  square_dis <- rep(0, n)
  for (j in 1:n) {
    square_dis[j] <- (t(z[j,]) %*% S_inv %*% z[j,])
  }

  # examine the outliter
  # in the chi-square plot, the outlier is farthers from the origin, test for 0.975 here
  checkpoint <- qchisq(0.975, df=p)
  outliers <- which(square_dis > checkpoint )

  return(list(standardized_values = z, generalized_squared_distance = square_dis, ouliters=outliters))
}
