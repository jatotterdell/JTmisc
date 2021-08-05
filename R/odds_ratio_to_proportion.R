#' Convert a baseline proportion and odds ratio to an updated
#'
#' @description
#' \loadmathjax
#' Uses the standard formula \mjeqn{p_2=ORp_1/(1 - p_1 + ORp_1)}{}.
#'
#' @param p The baseline proportion
#' @param OR The odds ratio
#' @return The proportion which results in an odds ratio of OR compared to p
#' @examples
#' odds_ratio_to_p(0.5, 2)
#' odds_ratio_to_p(0.5, 1/2)
odds_ratio_to_p <- function(p = 0.5, OR = 1) {
  return( p * OR / (1 - p + p * OR))
}
