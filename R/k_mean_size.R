#' Sample Size or Power Calculation for K Means
#'
#' Calculates the required sample size or achieved power for testing the difference between K means.
#' Supports two-sided and one-sided tests.
#'
#' @param muA Numeric. Mean of group A.
#' @param muB Numeric. Mean of group B.
#' @param kappa Numeric. Sample size ratio (nA / nB). Defaults to 1.
#' @param sd Numeric (optional). Standard deviation. Required for `"2-side"` tests.
#' @param sdA Numeric (optional). Standard deviation of group A. Required for `"1-side"` tests.
#' @param sdB Numeric (optional). Standard deviation of group B. Required for `"1-side"` tests.
#' @param tau Integer. Number of comparisons.
#' @param alpha Numeric. Type I error rate.
#' @param beta Numeric (optional). Type II error rate (1 - power). Required for sample size calculation.
#' @param n Integer (optional). Sample size. Required when calculating power for `"two-sided"` test.
#' @param nA Integer (optional). Sample size of group A. Required when calculating power for `"one-sided"` tests.
#' @param test_type Character. Type of test: `"2-side"`, `"1-side"`.
#'
#' @return
#' Numeric.
#' Returns the required sample size (if beta is given), or the power (if n* is given).
#'
#' @examples
#' # Required sample size for two-sided test with multiple comparisons
#' k_mean_size(muA = 105, muB = 100, sd = 15, tau = 3, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power calculation for one-sided test with different variances
#' k_mean_size(muA = 102, muB = 100, sdA = 10, sdB = 12, tau = 5, alpha = 0.05, nA = 30, test_type = "1-side")
#'
#' @export
k_mean_size <- function(muA, muB, kappa = 1, sd = NULL, sdA = NULL, sdB = NULL, tau, alpha, beta = NULL, n = NULL, nA = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling(2*(sd*(qnorm(1-alpha/(2/tau))+qnorm(1-beta))/(muA-muB))^2))
    } else if (test_type == "1-side") {
      return(ceiling((sdA^2+sdB^2/kappa)*((qnorm(1-alpha/tau)+qnorm(1-beta))/(muA-muB))^2))
    }
  } else if (!is.null(n)) {
    if (test_type == "2-side") {
      return(pnorm((muA-muB)/(sd*sqrt(2/n))-qnorm(1-alpha/(2/tau)))+pnorm(-(muA-muB)/(sd*sqrt(2/n))-qnorm(1-alpha/(2/tau))))
    }
  } else if (!is.null(nA)) {
    if (test_type == "1-side") {
      return(pnorm((muA-muB)/sqrt(sdA^2+sdB^2/kappa)*sqrt(nA)-qnorm(1-alpha/tau)))
    }
  }
}
