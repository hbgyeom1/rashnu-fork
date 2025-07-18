#' Sample Size or Power Calculation for Paired Proportion Test (McNemar's Test)
#'
#' This function calculates the required sample size or the achieved power for a paired-sample test of proportions,
#' such as McNemar's test for binary outcomes in matched pairs (e.g., before-and-after studies or matched case-control).
#'
#' @param p01 Numeric. Proportion of discordant pairs where the outcome changed from 0 to 1.
#' @param p10 Numeric. Proportion of discordant pairs where the outcome changed from 1 to 0.
#' @param alpha Numeric. Significance level (Type I error rate).
#' @param beta Numeric (optional). Type II error rate (1 - power). Required when computing required sample size.
#' @param n Integer (optional). Sample size (number of paired observations). Required when computing power.
#' @param test_type Character. Type of hypothesis test. Must be either `"2-side"` or `"1-side"`.
#'
#' @return
#' - If `beta` is provided and `n` is NULL, returns the required sample size (rounded up) to achieve the desired power.
#' - If `n` is provided and `beta` is NULL, returns the achieved power.
#'
#' @details
#' The test is based on the distribution of discordant pairs in a 2×2 contingency table for paired data.
#' The formula is derived using a normal approximation for McNemar’s test.
#' The variance includes a correction term for the squared difference of discordant proportions.
#'
#' @examples
#' # Calculate required sample size for two-sided McNemar test
#' pair_prop_size(p01 = 0.1, p10 = 0.2, alpha = 0.05, beta = 0.2, test_type = "2-side")
#'
#' # Calculate power for one-sided McNemar test
#' pair_prop_size(p01 = 0.05, p10 = 0.15, alpha = 0.05, n = 100, test_type = "1-side")
#'
#' @export
pair_prop_size <- function(p01, p10, alpha, beta = NULL, n = NULL, test_type = "2-side") {
  if (!is.null(beta)) {
    if (test_type == "2-side") {
      return(ceiling(((qnorm(1-alpha/2)*sqrt(p10+p01)+qnorm(1-beta)*sqrt(p10+p01-(p10-p01)^2))/(p10-p01))^2))
    } else if (test_type == "1-side") {
      return(ceiling(((qnorm(1-alpha)*sqrt(p10+p01)+qnorm(1-beta)*sqrt(p10+p01-(p10-p01)^2))/(p10-p01))^2))
    }
  } else if (!is.null(n)) {
    if (test_type == "2-side") {
      return(pnorm(((p10-p01)*sqrt(n)-qnorm(1-alpha/2)*sqrt(p10+p01))/sqrt(p10+p01-(p10-p01)^2))+pnorm((-(p10-p01)*sqrt(n)-qnorm(1-alpha/2)*sqrt(p10+p01))/sqrt(p10+p01-(p10-p01)^2)))
    } else if (test_type == "1-side") {
      return(pnorm((abs(p10-p01)*sqrt(n)-qnorm(1-alpha)*sqrt(p10+p01))/sqrt(p10+p01-(p10-p01)^2)))
    }
  }
}
