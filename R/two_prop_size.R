#' Sample Size or Power Calculation for Two-Sample Proportion Test
#'
#' This function calculates the required sample size or achieved power for comparing two independent population proportions.
#' It supports two-sided, one-sided, non-inferiority, and equivalence tests using normal approximation methods.
#'
#' @param pA Numeric. Proportion in group A (e.g., treatment group).
#' @param pB Numeric. Proportion in group B (e.g., control group).
#' @param delta Numeric (optional). Non-inferiority or equivalence margin. Required for `"non-inferiority"` and `"equivalence"` tests.
#' @param kappa Numeric. Ratio of sample sizes (nB/nA). Defaults to 1 (equal sample sizes).
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when calculating required sample size.
#' @param nB Integer (optional). Sample size for group B. Required when calculating power.
#' @param test_type Character. Type of hypothesis test. Must be one of `"2-side"`, `"1-side"`, `"non-inferiority"`, or `"equivalence"`.
#'
#' @return
#' - If `beta` is provided and `nB` is NULL, returns the required sample size for group B (rounded up), with group A size determined by `kappa`.
#' - If `nB` is provided and `beta` is NULL, returns the achieved power.
#'
#' @details
#' Assumes independent samples and uses normal approximation for hypothesis testing on proportions.
#' The `kappa` parameter allows for unequal sample sizes across the two groups (nB = kappa * nA).
#'
#' @examples
#' # Required sample size for two-sided test
#' two_prop_size(pA = 0.6, pB = 0.5, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power calculation for one-sided test
#' two_prop_size(pA = 0.55, pB = 0.5, alpha = 0.05, nB = 100, test_type = "1-side")
#'
#' @export
two_prop_size <- function(pA, pB, delta = NULL, kappa = 1, alpha, beta = NULL, nB = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha/2)+qnorm(1-beta))/(pA-pB))^2))
    } else if (test_type == "1-side") {
      return(ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta))/(pA-pB))^2))
    } else if (test_type == "non-inferiority") {
      return(ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta))/(pA-pB-delta))^2))
    } else if (test_type == "equivalence") {
      return(ceiling((pA*(1-pA)/kappa+pB*(1-pB))*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(pA-pB)-delta))^2))
    }
  } else if (!is.null(nB)) {
    if (test_type == "2-side") {
      return(pnorm((pA-pB)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha/2))+pnorm(-(pA-pB)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha/2)))
    } else if (test_type == "1-side") {
      return(pnorm(abs((pA-pB)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB))-qnorm(1-alpha)))
    } else if (test_type == "non-inferiority") {
      return(pnorm((pA-pB-delta)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha))+pnorm(-(pA-pB-delta)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*(pnorm((abs(pA-pB)-delta)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha))+pnorm(-(abs(pA-pB)-delta)/sqrt(pA*(1-pA)/nB/kappa+pB*(1-pB)/nB)-qnorm(1-alpha)))-1)
    }
  }
}
