#' Sample Size or Power Calculation for One-Sample Proportion Test
#'
#' This function calculates either the required sample size or the power for hypothesis testing on a single population proportion.
#' It supports two-sided, one-sided, non-inferiority, and equivalence tests, with normal approximation methods.
#'
#' @param p Numeric. The true population proportion under the alternative hypothesis.
#' @param p0 Numeric. The hypothesized proportion under the null hypothesis.
#' @param delta Numeric (optional). The non-inferiority or equivalence margin. Required for `"non-inferiority"` and `"equivalence"` tests.
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when calculating required sample size.
#' @param n Integer (optional). Sample size. Required when calculating power.
#' @param test_type Character. Type of hypothesis test. Must be one of `"2-side"`, `"1-side"`, `"non-inferiority"`, or `"equivalence"`.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required sample size (rounded up).
#' - If `n` is provided and `beta` is NULL, returns the achieved power.
#'
#' @details
#' The function uses normal approximations to calculate sample size and power.
#' The one-sided test uses a modified variance approach based on the null hypothesis proportion.
#'
#' @examples
#' # Required sample size for a two-sided test
#' one_prop_size(p = 0.55, p0 = 0.5, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power of a one-sided test with given sample size
#' one_prop_size(p = 0.6, p0 = 0.5, alpha = 0.05, n = 100, test_type = "1-side")
#'
#' @export
one_prop_size <- function(p, p0, delta = NULL, alpha, beta = NULL, n = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling(p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-beta))/(p-p0))^2))
    } else if (test_type == "1-side") {
      return(ceiling(p0*(1-p0)*((qnorm(1-alpha)+qnorm(1-beta)*sqrt(p*(1-p)/p0/(1-p0)))/(p-p0))^2))
    } else if (test_type == "non-inferiority") {
      return(ceiling(p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta))/(p-p0-delta))^2))
    } else if (test_type == "equivalence") {
      return(ceiling(p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(p-p0)-delta))^2))
    }
  } else if (!is.null(n)) {
    if (test_type == "2-side") {
      return(pnorm((p-p0)/sqrt(p*(1-p)/n)-qnorm(1-alpha/2))+pnorm(-(p-p0)/sqrt(p*(1-p)/n)-qnorm(1-alpha/2)))
    } else if (test_type == "1-side") {
      return(pnorm(sqrt(p0*(1-p0)/p/(1-p))*(abs((p-p0)/sqrt(p0*(1-p0)/n))-qnorm(1-alpha))))
    } else if (test_type == "non-inferiority") {
      return(pnorm((p-p0-delta)/sqrt(p*(1-p)/n)-qnorm(1-alpha))+pnorm(-(p-p0-delta)/sqrt(p*(1-p)/n)-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*(pnorm((abs(p-p0)-delta)/sqrt(p*(1-p)/n)-qnorm(1-alpha))+pnorm(-(abs(p-p0)-delta)/sqrt(p*(1-p)/n)-qnorm(1-alpha)))-1)
    }
  }
}
