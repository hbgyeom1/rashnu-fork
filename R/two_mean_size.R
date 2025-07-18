#' Sample Size or Power Calculation for Two-Sample Mean Test
#'
#' Calculates the required sample size or achieved power for testing the difference between two independent population means.
#' Supports two-sided, one-sided, non-inferiority, and equivalence tests.
#'
#' @param muA Numeric. Mean of group A
#' @param muB Numeric. Mean of group B
#' @param delta Numeric (optional). Margin for non-inferiority or equivalence tests. Required for `"non-inferiority"` and `"equivalence"` tests.
#' @param kappa Numeric. Sample size ratio (nA / nB). Defaults to 1.
#' @param sd Numeric (optional). Standard deviation. Required for `"2-side"`, `"non-inferiority"`, and `"equivalence"` tests.
#' @param sdA Numeric (optional). Standard deviation of group A. Required for `"1-side"` tests.
#' @param sdB Numeric (optional). Standard deviation of group B. Required for `"1-side"` tests.
#' @param alpha Numeric. Type I error rate.
#' @param beta Numeric (optional). Type II error rate (1 - power). Required for sample size calculation.
#' @param nA Integer (optional). Sample size of group A. Required when calculating power for `"1-side"` tests.
#' @param nB Integer (optional). Sample size of group B. Required when calculating power for `"2-side"`, `"non-inferiority"`, and `"equivalence"` tests.
#' @param test_type Character. Type of test: `"2-side"`, `"1-side"`, `"non-inferiority"`, or `"equivalence"`.
#'
#' @return
#' Numeric.
#' Returns the required sample size (if beta is given), or the power (if n* is given).
#'
#' @note
#' Only one of `beta` or `n*` should be specified. Supplying both will result in undefined behavior.
#' For `"2-side"`, `"non-inferiority"`, and `"equivalence"` tests, `sd` is required.
#' When calculating power for `"2-side"`, `"non-inferiority"`, and `"equivalence"` tests, `nB` is required.
#' For `"1-side"` tests, `sdA`, `sdB` are required.
#' When calculating power for `"1-side"` tests, `nA` is required.
#'
#' @examples
#' # Required sample size for two-sided test
#' two_mean_size(muA = 5, muB = 10, kappa = 1, sd = 10, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power of two-sided test
#' two_mean_size(muA = 5, muB = 10, kappa = 1, sd = 10, alpha = 0.05, nB = 63, test_type = "2-side")
#'
#' # Required sample size for one-sided test
#' two_mean_size(muA = 132.86, muB = 127.44, kappa = 2, sdA = 15.34, sdB = 18.23, alpha = 0.05, beta = 0.2, test_type = "1-side")
#'
#' # Power of one-sided test
#' two_mean_size(muA = 132.86, muB = 127.44, kappa = 2, sdA = 15.34, sdB = 18.23, alpha = 0.05, nA = 85, test_type = "1-side")
#'
#' # Required sample size for non-inferiority test
#' two_mean_size(muA = 5, muB = 5, delta = 5, kappa = 1, sd = 10, alpha = 0.05, beta = 0.2, test_type = "non-inferiority")
#'
#' # Power of non-inferiority test
#' two_mean_size(muA = 5, muB = 5, delta = 5, kappa = 1, sd = 10, alpha = 0.05, nB = 50, test_type = "non-inferiority")
#'
#' # Required sample size for equivalence test
#' two_mean_size(muA = 5, muB = 4, delta = 5, kappa = 1, sd = 10, alpha = 0.05, beta = 0.2, test_type = "equivalence")
#'
#' # Power of equivalence test
#' two_mean_size(muA = 5, muB = 4, delta = 5, kappa = 1, sd = 10, alpha = 0.05, nB = 108, test_type = "equivalence")
#'
#' @export
two_mean_size <- function(muA, muB, delta = NULL, kappa = 1, sd = NULL, sdA = NULL, sdB = NULL, alpha, beta = NULL, nA = NULL, nB = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling((1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2))
    } else if (test_type == "1-side") {
      return(ceiling((sdA^2+sdB^2/kappa)*((qnorm(1-alpha)+qnorm(1-beta))/(muA-muB))^2))
    } else if (test_type == "non-inferiority") {
      return(ceiling((1+1/kappa)*(sd*(qnorm(1-alpha)+qnorm(1-beta))/(muA-muB-delta))^2))
    } else if (test_type == "equivalence") {
      return(ceiling((1+1/kappa)*(sd*(qnorm(1-alpha)+qnorm(1-beta/2))/(abs(muA-muB)-delta))^2))
    }
  } else if (!is.null(nA)) {
    if (test_type == "1-side") {
      return(pnorm((muA-muB)/sqrt(sdA^2+sdB^2/kappa)*sqrt(nA)-qnorm(1-alpha)))
    }
  } else if (!is.null(nB)) {
    if (test_type == "2-side") {
      return(pnorm((muA-muB)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha/2))+pnorm(-(muA-muB)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha/2)))
    } else if (test_type == "non-inferiority") {
      return(pnorm((muA-muB-delta)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha))+pnorm(-(muA-muB-delta)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*(pnorm((abs(muA-muB)-delta)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha))+pnorm(-(abs(muA-muB)-delta)/(sd*sqrt((1+1/kappa)/nB))-qnorm(1-alpha)))-1)
    }
  }
}
