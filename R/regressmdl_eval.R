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
#' library("explainer")
#' seed <- 246
#' set.seed(seed)
#' # Load necessary packages
#' if (!requireNamespace("mlbench", quietly = TRUE)) stop("mlbench not installed.")
#' if (!requireNamespace("mlr3learners", quietly = TRUE)) stop("mlr3learners not installed.")
#' if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger not installed.")
#' # Load BreastCancer dataset
#' utils::data("BreastCancer", package = "mlbench")
#' mydata <- BreastCancer[, -1]
#' mydata <- na.omit(mydata)
#' sex <- sample(
#'   c("Male", "Female"),
#'   size = nrow(mydata),
#'   replace = TRUE
#' )
#' mydata$age <- sample(
#'   seq(18, 60),
#'   size = nrow(mydata),
#'   replace = TRUE
#' )
#' mydata$sex <- factor(
#'   sex,
#'   levels = c("Male", "Female"),
#'   labels = c(1, 0)
#' )
#' mydata$Class <- NULL
#' mydata$Cl.thickness <- as.numeric(mydata$Cl.thickness)
#' target_col <- "Cl.thickness"
#' maintask <- mlr3::TaskRegr$new(
#'   id = "my_regression_task",
#'   backend = mydata,
#'   target = target_col
#' )
#' splits <- mlr3::partition(maintask)
#' mylrn <- mlr3::lrn(
#'   "regr.ranger",
#'   predict_type = "response"
#' )
#' mylrn$train(maintask, splits$train)
#' regressmdl_eval_results <- regressmdl_eval(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits
#' )
#'
#' @references
#' Lang M, Binder M, Richter J, Schratz P, Pfisterer F, Coors S, Au Q, Casalicchio G, Kotthoff L, Bischl B. mlr3: A modern object-oriented machine learning framework in R. Journal of Open Source Software. 2019 Dec 11;4(44):1903.
#'
#' @seealso
#' [eCM_plot()]
#'
#' @keywords internal
#' @family regression evaluation
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
