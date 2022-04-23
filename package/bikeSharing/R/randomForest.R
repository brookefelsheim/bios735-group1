#' Train a random forest model on bike sharing data
#'
#' This function trains a random forest model on bike
#' sharing data using the Hour_chunks, Max_temp, and
#' Rain_or_snow predictors.
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Bike_count, Hour_chunks,
#'             Max_temp, and Rain_or_snow
#'
#' @return an object of class randomForest
#'
#' @import randomForest
#'
#' @export
trainRandomForest <- function(data) {

  if (!is.data.frame(data)) {
    stop("Input data is not a data frame")
  }
  if (!all(c("Bike_count", "Hour_chunks", "Max_temp", "Rain_or_snow") %in%
          colnames(seoul))) {
    stop("Input data does not contain all necessary columns:
         Bike_count, Hour_chunks, Max_temp, and Rain_or_snow")
  }

  model <- randomForest(Bike_count ~ Hour_chunks + Max_temp + Rain_or_snow,
                        data = data, importance=TRUE, ntree=500,
                        mtry = 3, do.trace=100)
  return(model)
}

#' Plot random forest variable importance
#'
#' This function plots the amount of increase in node
#' purity vs. the amount of increase in MSE that a
#' each variable in a random forest model provides
#'
#' @param model an object of class randomForest
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @import randomForest
#' @importFrom data.table setorder
#' @importFrom ggrepel geom_label_repel
#'
#' @export
plotRandomForestImportance <- function(model) {
  importance_matrix <- model$importance %>% as.data.frame(check.names=F)
  setorder(importance_matrix, -`%IncMSE`)
  g <- ggplot(importance_matrix,aes(x =`%IncMSE`, y = IncNodePurity)) +
    geom_point(color = "grey30", alpha = 0.7) +
    theme_article() +
    ggrepel::geom_label_repel(label = c(rownames(importance_matrix)),
                              nudge_x = 0.5, min.segment.length = 0) +
    xlab("Increase in MSE") +
    ylab("Increase in node purity")
}
