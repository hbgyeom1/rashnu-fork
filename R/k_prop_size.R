#' Sample Size or Power Calculation for Two-Sample Proportion Test with Multiple Comparisons
#'
#' This function calculates the required sample size or achieved power for comparing two independent proportions when multiple hypotheses are tested.
#' It adjusts the significance level using a Bonferroni-style correction based on the number of comparisons (`tau`).
#'
#' @param pA Numeric. Proportion in group A (e.g., treatment group).
#' @param pB Numeric. Proportion in group B (e.g., control group).
#' @param tau Integer. Number of comparisons (used to adjust alpha level: alpha / (2 * tau)).
#' @param alpha Numeric. Overall significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when computing required sample size.
#' @param n Integer (optional). Sample size per group. Required when computing power.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required sample size per group (rounded up) for desired power.
#' - If `n` is provided and `beta` is NULL, returns the achieved power of the test.
#'
#' @details
#' The function assumes independent binomial samples and uses a normal approximation.
#' The alpha level is adjusted for multiple comparisons using a Bonferroni correction (i.e., alpha / (2 * tau) for two-sided tests).
#'
#' @examples
#' # Required sample size for multiple comparisons (tau = 3)
#' k_prop_size(pA = 0.6, pB = 0.5, tau = 3, alpha = 0.05, beta = 0.2)
#'
#' # Achieved power with known sample size
#' k_prop_size(pA = 0.55, pB = 0.5, tau = 5, alpha = 0.05, n = 100)
#'
#' @export
k_prop_size <- function(pA, pB, tau, alpha, beta = NULL, n = NULL) {
  if (!is.null(beta)) {
    return(ceiling((pA*(1-pA)+pB*(1-pB))*((qnorm(1-alpha/2/tau)+qnorm(1-beta))/(pA-pB))^2))
  } else if (!is.null(n)) {
    return(pnorm((pA-pB)/sqrt(pA*(1-pA)/n+pB*(1-pB)/n)-qnorm(1-alpha/2/tau))+pnorm(-(pA-pB)/sqrt(pA*(1-pA)/n+pB*(1-pB)/n)-qnorm(1-alpha/2/tau)))
  }
}
