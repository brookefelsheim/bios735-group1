#' Check correct format of random forest model object
#'
#' This is a helper function to ensure that the random forest trained caret
#' object used as input to a function is properly formatted. Throws warnings
#' if formatting is incorrect.
#'
#' @param rf.model trained random forest caret object
#'              (output of train_random_forest() function)
#'
checkModelRF <- function(rf.model) {

  if (class(rf.model) != "train") {
    stop("rf.model is not of class train")
  }
  if (rf.model$method != "rf") {
    stop("rf.model$method is not 'rf'")
  }

}
