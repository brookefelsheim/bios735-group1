#' Negative binomial GLMM trained on London Year 1 data
#'
#' Fitted negative binomial generalized linear mixed model trained on the
#' Year 1 data from the London bike sharing dataset. This is included
#' as an accessible object from within the R package because the model
#' takes a long time to train.
#'
#' @format A list with 7 components:
#' \describe{
#'   \item{beta}{Fitted beta values, length: 14}
#'   \item{s2gamma}{Fitted s2gamma value}
#'   \item{theta}{Fitted theta value}
#'   \item{eps}{Final epsilon value}
#'   \item{qfunction}{Final Q function value}
#'   \item{day_ranef}{Random effect values for day of year, length: 365}
#'   \item{iter}{Number of iterations run}
#' }
"glmm_fit"
