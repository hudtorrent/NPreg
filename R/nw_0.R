#' Nadaraya-Watson nonparametric regression at a particular point x0.
#' 
#' @param x vector with values for the regressor variable;
#' @param y vector with values for the dependent variable;
#' @param x0 scalar where the user wants to evaluate the estimator;
#' @param h bandwidth (scalar);
#' @param k1 a kernel function;
#' @param ... specific parameters for k1.
#' @return scalar which is the Nadaraya-Watson estimate at x0.
#' @importFrom stats dnorm
#' @export
#' @examples
#' n <- 300
#' x <- runif(n, -1, 1)
#' e <- rnorm(n, 0, 0.5)
#' y <- sin(0.5 * pi * x) + e
#' f_nw_0(x = x, y = y, x0 = 0.5, h = 0.1)

f_nw_0 <- function(x, y, x0, h, k1 = dnorm, ...) {
  aux <- k1((x - x0) / h, ...)
  pesos <- aux / sum(aux)
  res <- sum(pesos * y)
  return(res)
}
