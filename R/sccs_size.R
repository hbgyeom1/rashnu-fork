#' Sample Size or Power Calculation for Self-Controlled Case Series (SCCS) Design
#'
#' This function calculates either the required number of cases (`n`) or the statistical power for Self-Controlled Case Series (SCCS) studies.
#' It uses a normal approximation to estimate sample size or power based on the relative incidence rate and time-at-risk.
#'
#' @param p Numeric. Relative incidence (i.e., incidence rate ratio between exposed and unexposed periods).
#' @param r Numeric. Proportion of the observation period that is considered at risk (i.e., exposure time fraction).
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when calculating required sample size.
#' @param n Integer (optional). Number of cases. Required when calculating power.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required number of cases (rounded up) to achieve the desired power.
#' - If `n` is provided and `beta` is NULL, returns the achieved power.
#'
#' @details
#' The SCCS design compares event rates during exposed and unexposed periods within the same individual, controlling for fixed confounders.
#' This function applies a normal approximation to estimate power or sample size, assuming the log-relative incidence follows a normal distribution.
#'
#' @examples
#' # Required number of cases for SCCS study
#' sccs_size(p = 1.5, r = 0.1, alpha = 0.05, beta = 0.2)
#'
#' # Power of SCCS study with 150 cases
#' sccs_size(p = 1.5, r = 0.1, alpha = 0.05, n = 150)
#'
#' @export
sccs_size <- function(p, r, alpha, beta = NULL, n = NULL) {
  if (!is.null(beta)) {
    return(ceiling((qnorm(1-alpha/2)+qnorm(1-beta)*(p*r+1-r)/sqrt(p))^2/(r*(1-r)*log(p)^2)))
  } else if (!is.null(n)) {
    return(pnorm((log(p)*sqrt(n*p*r*(1-r))-qnorm(1-alpha/2)*sqrt(p))/(p*r+1-r)))
  }
}
