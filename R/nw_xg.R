#' Nadaraya-Watson nonparametric regression over a grid.
#' @param x vector with values for the regressor variable;
#' @param y vector with values for the dependent variable;
#' @param xg vector where the user wants to evaluate the estimator;
#' @param h bandwidth (scalar);
#' @param k1 a kernel function;
#' @param ... specific parameters for k1.
#' @return vector with the Nadaraya-Watson estimates at xg.
#' @importFrom stats dnorm
#' @export
#' @examples
#' n <- 300
#' x <- runif(n, -1, 1)
#' e <- rnorm(n, 0, 0.5)
#' y <- sin(0.5 * pi * x) + e
#' f_nw_grid(x = x, y = y, xg = x, h = 0.1)

f_nw_grid <- function(x, y, xg, h, k1 = dnorm, ...) {

    ng <- length(xg)
    res <- rep(NA, ng)

    for (i in seq_len(ng)) {

        aux <- k1((x - xg[i]) / h, ...)
        pesos <- aux / sum(aux)
        res[i] <- sum(pesos * y)

    }

    return(res)
}
