#' findfirst
#'
#' Find the first element in a list which is TRUE
#'
#' @param x The vector of logicals
#' @param v The default value to return if no elements match TRUE
#' @return The first element in x which is TRUE, or v if none are TRUE
#'
#' @export
#'
findfirst <- function(x, v = NA) {
  j <- which(x)
  if(length(j)) min(j) else v
}


#' Multiply every column combination between two matrices
#'
#' Function is used for generating interactions between domain matrices.
#' Note that terms on matrix A change first, i.e. `out[, 1] = A[, 1]B[, 1]`, `out[, 2] = A[, 2]B[,1]` etc.
#'
#' @param A matrix 1
#' @param B matrix 2
#' @param sep Separation in column names.
#' @return Matrix of interactions between A and B
#'
#' @export
#'
colwise_mult <- function(A, B, sep = "") {
  out <- matrix(apply(A, 2, FUN = function(x) x * B), nrow(A), ncol(A) * ncol(B))
  rownames(out) <- rownames(A)
  colnames(out) <- paste(colnames(A), rep(colnames(B), each = ncol(A)), sep = sep)
  return(out)
}


#' Normalise vector to sum to 1
#'
#' @param x A numeric vector
#' @return The vector `x` normalised to sum to 1.
#'
#' @export
#'
normalise <- function(x) x / sum(x)
