#' Sample Size or Power Calculation for Cox Proportional Hazards Model
#'
#' This function calculates the required sample size or the achieved power for time-to-event analysis using the Cox proportional hazards model.
#' It supports two-sided, non-inferiority, and equivalence tests based on hazard ratios and event probabilities.
#'
#' @param hr Numeric. Hazard ratio under the alternative hypothesis.
#' @param hr0 Numeric (optional). Hazard ratio under the null hypothesis. Required for `"2-side"` and `"non-inferiority"` tests.
#' @param delta Numeric (optional). Equivalence margin on the log-hazard scale. Required for `"equivalence"` tests.
#' @param pE Numeric. Event rate (i.e., the proportion of subjects expected to experience the event).
#' @param pA Numeric. Proportion of participants in the active treatment group.
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when calculating required sample size.
#' @param n Integer (optional). Total sample size. Required when calculating power.
#' @param test_type Character. Type of hypothesis test. Must be one of `"2-side"`, `"non-inferiority"`, or `"equivalence"`.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required total sample size (rounded up).
#' - If `n` is provided and `beta` is NULL, returns the achieved power of the test.
#'
#' @details
#' The function uses the log hazard ratio scale and standard normal approximation.
#' The sample size formula includes a term for `pE`, the proportion expected to have the event, and `pA`, the allocation proportion for the active group.
#'
#' @examples
#' # Required sample size for a two-sided test
#' coxph_size(hr = 0.75, hr0 = 1, pE = 0.6, pA = 0.5, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Power of a non-inferiority test with given sample size
#' coxph_size(hr = 0.95, hr0 = 1, pE = 0.5, pA = 0.6, alpha = 0.025, n = 200, test_type = "non-inferiority")
#'
#' @export
coxph_size <- function(hr, hr0 = NULL, delta = NULL, pE, pA, alpha, beta = NULL, n = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling(((qnorm(1-alpha/2)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE)))
    } else if (test_type == "non-inferiority") {
      return(ceiling(((qnorm(1-alpha)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE)))
    } else if (test_type == "equivalence") {
      return(ceiling(((qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(log(hr))))^2/(pA*(1-pA)*pE)))
    }
  } else if (!is.null(n)) {
    if (test_type == "2-side") {
      return(pnorm((log(hr)-log(hr0))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha/2)))
    } else if (test_type == "non-inferiority") {
      return(pnorm((log(hr)-log(hr0))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha)))
    } else if (test_type == "equivalence") {
      return(2*pnorm((delta-abs(log(hr)))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha))-1)
    }
  }
}
