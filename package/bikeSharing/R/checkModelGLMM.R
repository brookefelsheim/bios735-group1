#' Check correct format of negative binomial mixed model object
#'
#' This is a helper function to ensure that the negative binomial mixed model
#' object used as input to a function is properly formatted. Throws warnings
#' if formatting is incorrect.
#'
#' @param glmm.model trained negative binomial mixed model
#'              (output of MCEM_algorithm() function)
#'
checkModelGLMM <- function(glmm.model) {

  if (!is.list(glmm.model)) {
    stop("glmm.model is not a list")
  }
  if (!is.numeric(glmm.model$beta)) {
    stop("glmm.model$beta is not numeric")
  }
  if (length(glmm.model$beta) != 14) {
    stop("glmm.model$beta is not length 14")
  }
  if (!is.numeric(glmm.model$s2gamma)) {
    stop("glmm.model$s2gamma is not numeric")
  }
  if (length(glmm.model$s2gamma) != 1) {
    stop("glmm.model$s2gamma is not length 1")
  }
  if (!is.numeric(glmm.model$theta)) {
    stop("glmm.model$theta is not numeric")
  }
  if (length(glmm.model$theta) != 1) {
    stop("glmm.model$theta is not length 1")
  }
  if (!is.numeric(glmm.model$eps)) {
    stop("glmm.model$eps is not numeric")
  }
  if (length(glmm.model$eps) != 1) {
    stop("glmm.model$eps is not length 1")
  }
  if (!is.numeric(glmm.model$qfunction)) {
    stop("glmm.model$qfunction is not numeric")
  }
  if (length(glmm.model$qfunction) != 1) {
    stop("glmm.model$qfunction is not length 1")
  }
  if (!is.numeric(glmm.model$day_ranef)) {
    stop("glmm.model$day_ranef is not numeric")
  }
  if (length(glmm.model$day_ranef) != 365) {
    stop("glmm.model$day_ranef is not length 365")
  }
  if (!is.numeric(glmm.model$iter)) {
    stop("glmm.model$iter is not numeric")
  }
  if (length(glmm.model$iter) != 1) {
    stop("glmm.model$iter is not length 1")
  }
}
