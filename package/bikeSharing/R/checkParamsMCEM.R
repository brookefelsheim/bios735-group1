#' Check correct format of MCEM parameters
#'
#' This is a helper function to ensure that the correct parameters (other
#' than data) are given to the MCEM algorithm function before running.
#'
#' @param beta_initial initial beta values (vector of 14 numeric values)
#' @param theta_initial initial theta value (single numeric value)
#' @param s2gamma_initial initial value for s2gamma (single numeric value)
#' @param M length of chain (single numeric count value)
#' @param burn.in the number of beginning iteration samples to discard
#'                (single numeric count value)
#' @param tol tolerance for convergence (single numeric value)
#' @param maxit maximum number of iterations to run (single numeric count value)
#' @param trace level of detailed output (single numeric value)
#'
checkParamsMCEM <- function(beta_initial, theta_initial, s2gamma_initial,
                            M, burn.in, tol, maxit, trace) {

  if (!is.numeric(beta_initial)) {
    stop("beta_initial is not numeric")
  }
  if (length(beta_initial) != 14) {
    stop("length of beta_initial is not 14")
  }
  if (!is.numeric(theta_initial)) {
    stop("theta_initial is not numeric")
  }
  if (length(theta_initial) != 1) {
    stop("length of theta_initial is not 1")
  }
  if (!is.numeric(s2gamma_initial)) {
    stop("s2gamma_initial is not numeric")
  }
  if (length(s2gamma_initial) != 1) {
    stop("length of s2gamma_initial is not 1")
  }
  if (!(all.equal(M, as.integer(M)))) {
    stop("M is not a whole number")
  }
  if (length(M) != 1) {
    stop("length of M is not 1")
  }
  if (!is.numeric(all.equal(burn.in, as.integer(burn.in)))) {
    stop("burn.in is not a whole number")
  }
  if (length(burn.in) != 1) {
    stop("length of burn.in is not 1")
  }
  if (!is.numeric(tol)) {
    stop("tol is not numeric")
  }
  if (length(tol) != 1) {
    stop("length of tol is not 1")
  }
  if (!is.numeric(all.equal(maxit, as.integer(maxit)))) {
    stop("maxit is not a whole number")
  }
  if (length(M) != 1) {
    stop("length of maxit is not 1")
  }
  if (!is.numeric(trace)) {
    stop("trace is not numeric")
  }
  if (length(trace) != 1) {
    stop("length of trace is not 1")
  }

}
