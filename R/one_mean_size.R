#' Sample Size or Power Calculation for One-Sample Mean Test
#'
#' Calculates the required sample size or the achieved power for testing a single population mean.
#' Supports two-sided, one-sided, non-inferiority, and equivalence tests.
#'
#' @param mu Numeric. True mean under the alternative hypothesis.
#' @param mu0 Numeric. Mean under the null hypothesis.
#' @param delta Numeric (optional). Margin for non-inferiority or equivalence tests. Required for `"non-inferiority"` and `"equivalence"` tests.
#' @param sd Numeric. Standard deviation.
#' @param alpha Numeric. Type I error rate.
#' @param beta Numeric (optional). Type II error rate (1 - power). Required for sample size calculation.
#' @param n Integer (optional). Sample size. Required when calculating power
#' @param test_type Character. Type of test: "2-side", "1-side", "non-inferiority", or "equivalence".
#'
#' @return
#' Numeric.
#' Returns the required sample size (if beta is given), or the power (if n is given).
#'
#' @note
#' Only one of `beta` or `n` should be specified. Supplying both will result in undefined behavior.
#'
#' @examples
#' # Required sample size for two-sided test
#' one_mean_size(mu = 2, mu0 = 1.5, sd = 1, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power of two-sided test
#' one_mean_size(mu = 2, mu0 = 1.5, sd = 1, alpha = 0.05, n = 32, test_type = "2-side")
#'
#' # Required sample size for one-sided test
#' one_mean_size(mu = 115, mu0 = 120, sd = 24, alpha = 0.05, beta = 0.2, test_type = "1-side")
#'
#' # Power of one-sided test
#' one_mean_size(mu = 115, mu0 = 120, sd = 24, alpha = 0.05, n = 143, test_type = "1-side")
#'
#' # Required sample size for non-inferiority test
#' one_mean_size(mu = 2, mu0 = 1.5, delta = -0.5, sd = 1, alpha = 0.05, beta = 0.2, test_type = "non-inferiority")
#'
#' # Power of non-inferiority test
#' one_mean_size(mu = 2, mu0 = 1.5, delta = -0.5, sd = 1, alpha = 0.05, n = 7, test_type = "non-inferiority")
#'
#' # Required sample size for equivalence test
#' one_mean_size(mu = 2, mu0 = 2, delta = 0.05, sd = 0.1, alpha = 0.05, beta = 0.2, test_type = "equivalence")
#'
#' # Power of equivalence test
#' one_mean_size(mu = 2, mu0 = 2, delta = 0.05, sd = 0.1, alpha = 0.05, n = 35, test_type = "equivalence")
#'
#' @export
one_mean_size <- function(mu, mu0, delta = NULL, sd, alpha, beta = NULL, n = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling((sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(mu-mu0))^2))
    } else if (test_type == "1-side") {
      return(ceiling((sd*(qnorm(1-alpha)+qnorm(1-beta))/(mu-mu0))^2))
    } else if (test_type == "non-inferiority") {
      return(ceiling((sd*(qnorm(1-alpha)+qnorm(1-beta))/(mu-mu0-delta))^2))
    } else if (test_type == "equivalence") {
      return(ceiling((sd*(qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(mu-mu0)))^2))
    }
  } else if (!is.null(n)) {
    if (test_type == "2-side") {
      return(pnorm((mu-mu0)/sd*sqrt(n)-qnorm(1-alpha/2))+pnorm(-(mu-mu0)/sd*sqrt(n)-qnorm(1-alpha/2)))
    } else if (test_type == "1-side") {
      return(pnorm(abs((mu-mu0)/sd*sqrt(n))-qnorm(1-alpha)))
    } else if (test_type == "non-inferiority") {
      return(pnorm((mu-mu0-delta)/sd*sqrt(n)-qnorm(1-alpha))+pnorm(-(mu-mu0-delta)/sd*sqrt(n)-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*(pnorm((abs(mu-mu0)-delta)/sd*sqrt(n)-qnorm(1-alpha))+pnorm(-(abs(mu-mu0)-delta)/sd*sqrt(n)-qnorm(1-alpha)))-1)
    }
  }
}
