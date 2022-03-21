#' expit(x)
#'
#' @description
#' Calculate expit(x)
#'
#' @importFrom stats qnorm
#'
#' @param x The value to transform
#' @return expit(x)
expit <- function(x) {
  1 / (1 + exp(-x))
}


#' logit(x)
#'
#' @description
#' Calculate logit(x)
#'
#' @importFrom stats qnorm
#'
#' @param x The value to transform
#' @return logit(x)
logit <- function(x) {
  log(x) - log(1 - x)
}


#' cumulative_expit(x)
#'
#' @description
#' Transform from cumulative logit to probability scale.
#'
#' @param x The vector of cut-points
#' @return cumulative_expit(x)
cumulative_expit <- function(x) {
  K <- length(x) + 1
  out <- numeric(K)
  out[1] <- 1 - expit(-x[1])
  out[K] <- expit(-x[K-1])
  if(K > 2) {
    for(k in seq_len(K-1)[-1]) {
      out[k] <- expit(-x[k-1]) - expit(-x[k])
    }
  }
  return(out)
}


#' cumulative_logit(x)
#'
#' @description
#' Transform from probabilities to cumulative logit cut-points.
#'
#' @param x The vector of probabilities
#' @return cumulative_logit(x)
cumulative_logit <- function(x) {
  return(logit(cumsum(x))[seq_len(length(x) - 1)])
}
