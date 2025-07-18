#' Sample Size or Power Calculation for One-Sample Binomial Proportion Test (Two-Sided)
#'
#' This function calculates either the required sample size or the achieved power for a two-sided test of a single binomial proportion.
#' It uses a normal approximation to the binomial distribution for testing whether the true proportion differs from a hypothesized value.
#'
#' @param p Numeric. The true proportion under the alternative hypothesis.
#' @param p0 Numeric. The hypothesized proportion under the null hypothesis.
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when calculating the required sample size.
#' @param n Integer (optional). Sample size. Required when calculating power.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required sample size (rounded up).
#' - If `n` is provided and `beta` is NULL, returns the achieved power of the test.
#'
#' @details
#' The function uses the normal approximation to the binomial distribution. It assumes a two-sided z-test for a single binomial proportion.
#'
#' @examples
#' # Required sample size for testing p = 0.55 vs p0 = 0.5
#' one_bino_size(p = 0.55, p0 = 0.5, alpha = 0.05, beta = 0.2)
#'
#' # Power with fixed sample size
#' one_bino_size(p = 0.55, p0 = 0.5, alpha = 0.05, n = 100)
#'
#' @export
one_bino_size <- function(p, p0, alpha, beta = NULL, n = NULL) {
  if (!is.null(beta)) {
    return(ceiling(p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-beta))/(p-p0))^2))
  } else if (!is.null(n)) {
    return(pnorm((p-p0)/sqrt(p*(1-p)/n)-qnorm(1-alpha/2))+pnorm(-(p-p0)/sqrt(p*(1-p)/n)-qnorm(1-alpha/2)))
  }
}
