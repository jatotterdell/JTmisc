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


#' cumulative_logit(x)
#'
#' @description
#' Transform from probabilities to cumulative logit cut-points.
#'
#' @param x The vector of probabilities
#' @return cumulative_logit(x)
#' @export
cumulative_logit <- function(x) {
    return(logit(cumsum(x))[seq_len(length(x) - 1)])
}


#' @title Ordered logistic density
#' @description
#' @param x Value
#' @param alpha Cut-points
#' @param eta Shift in alpha, default = 0
#' @param log Return log density, deafult FALSE
#' @return Ordered logistic density function
#' @export
dordlogit <- function(x, alpha, eta = 0, log = FALSE) {
    K <- length(alpha) + 1
    stopifnot("x must be between 1 and K" = all(x > 0 & x < K + 1))
    alpha <- c(-Inf, as.numeric(alpha), Inf)
    p <- inv_logit(alpha[x + 1] - eta) - inv_logit(alpha[x] - eta)
    if (log == TRUE) p <- log(p)
    return(p)
}


rordlogit <- function(n, phi = 0, a) {
    a <- c(as.numeric(a), Inf)
    k <- 1:length(a)
    if (length(phi) == 1) {
        p <- dordlogit(k, a = a, phi = phi, log = FALSE)
        y <- sample(k, size = n, replace = TRUE, prob = p)
    } else {
        # vectorized input
        y <- rep(NA, n)
        if (n > length(phi)) {
            # need to repeat some phi values
            phi <- rep(phi, ceiling(n / length(phi)))
        }
        for (i in 1:n) {
            p <- dordlogit(k, a = a, phi = phi[i], log = FALSE)
            y[i] <- sample(k, size = 1, replace = TRUE, prob = p)
        }
    }
    y
}
