#' Function to calculate the critical prior interval (CPI).
#'
#' @description
#' \loadmathjax
#' The critical prior interval is determined according to the *Principle of Fair-Minded Challenge*.
#' If results are "statistically-significant", then fair-minded *scepticism* applies,
#' and if results are "not statistically-significant", then fair-minded *advocacy* applies.
#' @details
#' Assuming a Normal prior with mean \mjeqn{\mu_0}{mu_0} and variance \mjeqn{\phi_0}{phi_0}, a *scepticism limit* (SL)
#' or *advocacy limit* (AL) is calculated such that the resulting posterior distribution (assuming a Normal likelihood)
#' renders the findings no longer credible or credible respectively (in reference to the location of the credible intervals).
#'
#' @references
#' Matthews RAJ. 2018 Beyond‘significance’:
#' principles and practice of the Analysis of Credibility.
#' *R.Soc.opensci.* **5**: 171047.http://dx.doi.org/10.1098/rsos.171047
#'
#' @param L The lower CI limit
#' @param U The upper CI limit
#' @param alpha The size which determines the target coverage of the confidence interval
#' @return Either the scepticisim limit (SL) or the advocacy limit (AL) depending on the values of L and U.
critical_prior_interval <- function(L, U, alpha = 0.05) {
  if(L > 0 | U < 0) {
    SL <- (U - L) ^ 2 / (4 * sqrt(U * L))
    mu0 <- 0
    phi0 <- ((SL - mu0) / qnorm(1 - alpha / 2))^2
    return(c(SL = SL, LP = -SL, UP = SL, mu0 = mu0, phi0 = phi0))
  } else {
    AL <- -(U + L)*(U - L)^2 / (2*U*L)
    mu0 <- AL / 2
    phi0 <- ((AL - mu0) / qnorm(1 - alpha/2))^2
    return(c(AL = AL, LP = 0, UP = AL, mu0 = mu0, phi0 = phi0))
  }
}
