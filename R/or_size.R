#' Sample Size or Power Calculation for Odds Ratio Tests
#'
#' This function computes the required sample size or the achieved power for hypothesis tests comparing two independent proportions
#' using the odds ratio. It supports equality, non-inferiority, and equivalence tests based on the logarithm of the odds ratio.
#'
#' @param pA Numeric. Proportion in group A (e.g., treatment group).
#' @param pB Numeric. Proportion in group B (e.g., control group).
#' @param delta Numeric (optional). Non-inferiority or equivalence margin on the log-odds scale. Required for `"non-inferiority"` and `"equivalence"` tests.
#' @param kappa Numeric. Ratio of sample sizes (nB/nA). Defaults to 1 (equal sample sizes).
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when computing required sample size.
#' @param nB Integer (optional). Sample size for group B. Required when computing power.
#' @param test_type Character. Type of hypothesis test. Must be one of `"equality"`, `"non-inferiority"`, or `"equivalence"`.
#'
#' @return
#' - If `beta` is provided and `nB` is NULL, returns the required sample size for group B (rounded up); group A size is determined by `kappa`.
#' - If `nB` is provided and `beta` is NULL, returns the achieved power of the test.
#'
#' @details
#' The function uses a normal approximation for the distribution of the log-odds ratio. The sample size calculation accounts for group allocation ratio (`kappa`)
#' and variance of the log-odds estimate. The margin `delta` must be specified for non-inferiority and equivalence designs.
#'
#' @examples
#' # Required sample size for odds ratio equality test
#' or_size(pA = 0.6, pB = 0.5, alpha = 0.05, beta = 0.2, test_type = "equality")
#'
#' # Power calculation for equivalence test
#' or_size(pA = 0.55, pB = 0.5, delta = 0.2, alpha = 0.05, nB = 100, test_type = "equivalence")
#'
#' @export
or_size <- function(pA, pB, delta = NULL, kappa = 1, alpha, beta = NULL, nB = NULL, test_type = "equality") {
  if (!is.null(beta)) {
    if (test_type == "equality") {
      return(ceiling((1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha/2)+qnorm(1-beta))/log(pA*(1-pB)/pB/(1-pA)))^2))
    } else if (test_type == "non-inferiority") {
      return(ceiling((1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta))/(log(pA*(1-pB)/pB/(1-pA))-delta))^2))
    } else if (test_type == "equivalence") {
      return(ceiling((1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(log(pA*(1-pB)/pB/(1-pA)))-delta))^2))
    }
  } else if (!is.null(nB)) {
    if (test_type == "equality") {
      return(pnorm(log(pA*(1-pB)/pB/(1-pA))*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha/2))+pnorm(-log(pA*(1-pB)/pB/(1-pA))*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha/2)))
    } else if (test_type == "non-inferiority") {
      return(pnorm((log(pA*(1-pB)/pB/(1-pA))-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha))+pnorm(-(log(pA*(1-pB)/pB/(1-pA))-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*(pnorm((abs(log(pA*(1-pB)/pB/(1-pA)))-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha))+pnorm(-(abs(log(pA*(1-pB)/pB/(1-pA)))-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))-qnorm(1-alpha)))-1)
    }
  }
}
