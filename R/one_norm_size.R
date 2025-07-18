#' Sample Size or Power Calculation for One-Sample Normal Test (Two-Sided)
#'
#' This function computes the required sample size or the achieved power for a two-sided test of a single population mean,
#' assuming normally distributed data with known standard deviation.
#'
#' @param mu Numeric. The true population mean under the alternative hypothesis.
#' @param mu0 Numeric. The mean under the null hypothesis.
#' @param sd Numeric. The known standard deviation of the population.
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when computing required sample size.
#' @param n Integer (optional). Sample size. Required when computing power.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required sample size (rounded up).
#' - If `n` is provided and `beta` is NULL, returns the achieved power of the test.
#'
#' @details
#' This function assumes a two-sided z-test for a single mean with known variance using normal approximation.
#'
#' @examples
#' # Calculate required sample size
#' one_norm_size(mu = 105, mu0 = 100, sd = 15, alpha = 0.05, beta = 0.2)
#'
#' # Calculate power with fixed sample size
#' one_norm_size(mu = 105, mu0 = 100, sd = 15, alpha = 0.05, n = 50)
#'
#' @export
one_norm_size <- function(mu, mu0, sd, alpha, beta = NULL, n = NULL) {
  if (!is.null(beta)) {
    return(ceiling((sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(mu-mu0))^2))
  } else if (!is.null(n)) {
    return(pnorm((mu-mu0)/sd*sqrt(n)-qnorm(1-alpha/2))+pnorm(-(mu-mu0)/sd*sqrt(n)-qnorm(1-alpha/2)))
  }
}
