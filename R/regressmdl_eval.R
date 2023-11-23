#' @title Regression Model Evaluation
#' @description
#' Provides calculations of measures to evaluate regression models.
#'
#' @param task mlr3 regression task object
#' @param trained_model mlr3 trained learner (model) object
#' @param splits mlr3 object defining data splits for train and test sets
#'
#' @importFrom stats cor
#' @export
#'
#' @return Data frame containing regression evaluation measures
#'
#' @examples
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
regressmdl_eval <- function(task,
                            trained_model,
                            splits) {
  # Extract the test set from the data
  mydata <- task$data()
  mydata <- as.data.frame(mydata)
  y <- mydata[, task$target_names][splits$test]
  test_set <- task$data(splits$test)

  # Extract predicted values and actual values from the test set
  pred_values <- trained_model$predict_newdata(test_set)$response
  actual_values <- y
  # Calculate evaluation measures
  mse <- mean((pred_values - actual_values)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(pred_values - actual_values))
  r_squared <- cor(pred_values, actual_values)^2

  # Create a data.frame to store the results
  results <- data.frame(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared
  )

  return(results)
}
