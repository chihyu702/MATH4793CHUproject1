#'
#' return the Q-Q plot of the dataset
#'
#' @param data the data going use to make Q-Q plot
#'
#' @return the Q-Q plot of the data
#' @export
#'
#' @importFrom stats rnorm
#' @importFrom stats qqnorm
#' @importFrom stats qqline
#'
#' @examples getQQplot(data = rnorm(100, mean = 0, sd = 1))
getQQplot <- function(data = rnorm(100, mean = 0, sd = 1)){
  qqnorm(data)
  qqline(data, col = "red")
}
