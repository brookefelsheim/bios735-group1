#' Train a random forest model on bike sharing data
#'
#' This function trains a random forest model on bike
#' sharing data using the Hour_chunks, Max_temp, and
#' Rain_or_snow predictors.
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Bike_count, Hour_chunks,
#'             Max_temp, Rain_or_snow, and Date
#'
#' @return an object of class train containing the
#'         fit random forest model
#'
#' @import caret
#'
#' @export
train_random_forest <- function(data) {

  checkBikeData(data)

  x = subset(data,
             select = c("Hour_chunks", "Is_weekend", "Is_holiday", 
                        "Season", "Min_temp", "Max_temp", "Min_humidity", "Max_humidity",
                        "Wind_speed", "Rain_or_snow", "Date"))
  
  y = data$Bike_count

  trCtl <- trainControl(method="cv", number=5, savePredictions=TRUE)
  rf.fit <- train(x, y, method="rf", trControl=trCtl)

  return(rf.fit)
}

#' Plot random forest variable importance
#'
#' This function plots the amount of increase in node
#' purity vs. the amount of increase in MSE that a
#' each variable in a random forest model provides
#'
#' @param data pre-processed bike sharing data frame
#'             with columns Bike_count, Hour_chunks,
#'             Max_temp, Rain_or_snow, and Date
#' @param mtry the mtry value from the model$bestTune
#'             parameter of the trained random forest
#'             model using the trainRandomForest(data)
#'
#' @return a ggplot object
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom randomForest randomForest
#' @importFrom data.table setorder
#' @importFrom ggrepel geom_label_repel
#'
#' @export
plot_rf_importance <- function(data, mtry = 4) {

  checkBikeData(data)

  model <- randomForest(Bike_count ~ Hour_chunks + Is_weekend + Is_holiday + Season + Min_temp + 
                          Max_temp + Min_humidity + Max_humidity + Wind_speed + Rain_or_snow,
                         data = data, importance = TRUE, ntree = 500, mtry = mtry)

  importance_matrix <- model$importance %>% as.data.frame(check.names = F)
  setorder(importance_matrix, -`%IncMSE`)

  g <- ggplot(importance_matrix,aes(x =`%IncMSE`, y = IncNodePurity)) +
    geom_point(color = "grey30", alpha = 0.7) +
    theme_article() +
    ggrepel::geom_label_repel(label = c(rownames(importance_matrix)),
                              nudge_x = 0.5, min.segment.length = 0) +
    xlab("Increase in MSE") +
    ylab("Increase in node purity")

  return(g)
}
