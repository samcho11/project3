#' Random Forest Cross-Validation
#'
#' This function splits the data into k groups for evaluating test set and
#' training sets to predict the best value by random forest algorithm
#'
#' @param k Number of folds
#'
#' @keywords prediction
#'
#' @return Average mean squared error across all \code{k} folds.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit predict pt sd
#' @importFrom dplyr filter
#' @importFrom class knn
#' @importFrom randomForest randomForest
#'
#' @examples
#' my_rf_cv(5)
#'
#'
#' @export
my_rf_cv <- function(k) {
  # Define folds
  penguins <- STAT302package::my_penguins
  fold <- sample(rep(1:k, length = nrow(penguins)))
  split_data <- data.frame(penguins, "split" = fold)
  split_data <- na.omit(split_data)

  mse <- rep(NA, 5)
  for (i in 1:k) {
    # Use k-fold to assign training and test data

    #data_train <- split_data %>% filter(fold != i)
    #data_train <- subset(data_train, select = -fold)

    #data_test <- split_data %>% filter(fold == i)
    #data_test <- subset(data_test, select = -fold)

    data_train <- split_data %>% filter(split != i)
    data_test <- split_data %>% filter(split == i)

    # Train a random forest model with 100 trees to predict body mass
    rf_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm +
                               flipper_length_mm, data = data_train, ntree = 100)
    # Predict the body mass
    pred <- predict(rf_model, data_test[, -1])

    # Calculate the MSE, the average squared difference between prediction and actual value
    mse[i] <- mean((pred - data_test$body_mass_g)^2)
  }

  # Calculate the mean MSE
  avg_mse <- mean(mse)
  return(avg_mse)
}
