# Bernoulli distribution ----

#' @title Bernoulli distribution
#' @description Calculate PMF for Bernoulli distribution
#' @param x Value
#' @param prob Probability parameter
#' @param log Return log PMF, `FALSE` by default
#' @export
dbern <- function(x, prob, log = FALSE) {
    stats::dbinom(x, size = 1, prob = prob, log = log)
}


#' @title Bernoulli distribution
#' @description Random draw from Bernoulli distribution
#' @param n Number of draws
#' @param prob Probability parameter
#' @return Random draws
#' @export
rbern <- function(n, prob = 0.5) {
    stats::rbinom(n, size = 1, prob = prob)
}


# Categorical distribution ----

#' @title Categorical distribution
#' @description
#' Random generation for categorical distribution.
#' @param n number of observations.
#' @param prob vector or matrix of probabilities where nrow(prob) = n.
#' @return Random deviate
#' @export
rcategorical <- function(n, prob = c(0.5, 0.5)) {
    if ("matrix" %in% class(prob)) {
        stopifnot("nrow(prob) != n" = n == nrow(prob))
        k <- ncol(prob)
        y <- sapply(1:n, \(x) sample(seq_len(k), size = 1, prob = prob[x, ]))
    } else {
        k <- length(prob)
        y <- sample(seq_len(k), size = n, prob = prob, replace = TRUE)
    }
    return(y)
}


# Cumulative logistic distribution ----

#' @title expit(x)
#' @description
#' Calculate expit(x)
#' @param x The value to transform
#' @return expit(x)
#' @export
expit <- function(x) {
    1 / (1 + exp(-x))
}


#' @title logit(x)
#' @description
#' Calculate logit(x)
#' @param x The value to transform
#' @return logit(x)
#' @export
logit <- function(x) {
    log(x) - log(1 - x)
}


#' @title inv_logit(x)
#' @description
#' Calculate inv_logit(x), an alias for expit(x).
#' @param x The value to transform
#' @return expit(x)
#' @export
inv_logit <- function(x) {
    expit(x)
}


#' cumulative_expit(x)
#'
#' @description
#' Transform from cumulative logit to probability scale.
#'
#' @param x The vector of cut-points
#' @return cumulative_expit(x)
#' @export
cumulative_expit <- function(x, reverse = FALSE) {
    K <- length(x) + 1
    out <- numeric(K)
    if (reverse) {
    } else {
        out[1] <- expit(x[1])
        out[K] <- 1 - expit(x[K - 1])
        if (K > 2) {
            for (k in seq_len(K - 1)[-1]) {
                out[k] <- expit(x[k]) - expit(x[k - 1])
            }
        }
    }
    return(out)
}


#' @title Cumulative logistic function
#' @description
#' Transform from probabilities to cumulative logit cut-points.
#' @param x The vector of probabilities
#' @return cumulative_logit(x)
#' @export
cumulative_logit <- function(x) {
    return(logit(cumsum(x))[seq_len(length(x) - 1)])
}


# #' @title Ordered logistic density
# #' @description
# #' @param x Value
# #' @param alpha Cut-points
# #' @param eta Shift in alpha, default = 0
# #' @param log Return log density, deafult FALSE
# #' @return Ordered logistic density function
# #' @export
# dordlogit <- function(x, alpha, eta = 0, log = FALSE) {
#     K <- length(alpha) + 1
#     stopifnot("x must be between 1 and K" = all(x > 0 & x < K + 1))
#     alpha <- c(-Inf, as.numeric(alpha), Inf)
#     p <- inv_logit(alpha[x + 1] - eta) - inv_logit(alpha[x] - eta)
#     if (log == TRUE) p <- log(p)
#     return(p)
# }


# rordlogit <- function(n, alpha, eta = 0) {
#     K <- length(alpha) + 1
#     if (length(eta) == 1) {
#         p <- dordlogit(seq_len(K), alpha, eta, log = FALSE)
#         y <- rcategorical(n, p)
#     } else {
#         y <- rep(NA, n)
#         stopifnot("length(eta) not equal to n" = n == length(eta))
#         for (i in 1:n) {
#             p <- dordlogit(seq_len(K), alpha, eta, log = FALSE)
#             y[i] <- rcategorical(n, p)
#         }
#     }
#     y
# }
