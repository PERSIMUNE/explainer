#' @title SHAP Partial Plot
#' @description Generates an interactive partial dependence plot based on SHAP values, visualizing the marginal effect of one or two features on the predicted outcome of a machine learning model.
#'
#' @param shap_Mean_long data frame containing SHAP values in long format
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap
#' @importFrom egg theme_article
#' @importFrom plotly ggplotly
#'
#' @return an interactive partial dependence plot
#' @export
#'
#' @examples
#' \donttest{
#' library("explainer")
#' seed <- 246
#' set.seed(seed)
#' # Load necessary packages
#' if (!requireNamespace("mlbench", quietly = TRUE)) stop("mlbench not installed.")
#' if (!requireNamespace("mlr3learners", quietly = TRUE)) stop("mlr3learners not installed.")
#' if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger not installed.")
#' # Load BreastCancer dataset
#' utils::data("BreastCancer", package = "mlbench")
#' target_col <- "Class"
#' positive_class <- "malignant"
#' mydata <- BreastCancer[, -1]
#' mydata <- na.omit(mydata)
#' sex <- sample(
#'   c("Male", "Female"),
#'   size = nrow(mydata),
#'   replace = TRUE
#' )
#' mydata$age <- as.numeric(sample(
#'   seq(18,60),
#'   size = nrow(mydata),
#'   replace = TRUE
#' ))
#' mydata$sex <- factor(
#'   sex,
#'   levels = c("Male", "Female"),
#'   labels = c(1, 0)
#' )
#' maintask <- mlr3::TaskClassif$new(
#'   id = "my_classification_task",
#'   backend = mydata,
#'   target = target_col,
#'   positive = positive_class
#' )
#' splits <- mlr3::partition(maintask)
#' mylrn <- mlr3::lrn(
#'   "classif.ranger",
#'   predict_type = "prob"
#' )
#' mylrn$train(maintask, splits$train)
#' SHAP_output <- eSHAP_plot(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   sample.size = 2, # also 30 or more
#'   seed = seed,
#'   subset = 0.02 # up to 1
#' )
#' shap_Mean_long <- SHAP_output[[3]]
#' myplot <- ShapPartialPlot(shap_Mean_long)
#' }
ShapPartialPlot <- function(shap_Mean_long) {
  # utils::globalVariables(c("f_val", "pred_prob", "correct_prediction"))
  f_val <- NULL
  pred_prob <- NULL
  correct_prediction <- NULL
  shapPartialPlot <- ggplot(shap_Mean_long, aes(x = f_val, y = pred_prob)) +
    geom_point(aes(shape = correct_prediction), size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x) +
    ggplot2::xlab("Normalized Feature Values [0 1]") +
    ggplot2::ylab("Predicted Probability for the Positive Class") +
    facet_wrap(~ feature, scales = "free_y") +
    egg::theme_article()

  shapPartialPlot <- ggplotly(shapPartialPlot)
  # Display the plot
  return(shapPartialPlot)
}
