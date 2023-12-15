#' @title SHAP Values versus Feature Values
#' @description SHAP values in association with feature values
#'
#' @param shap_Mean_long the data frame containing SHAP values in long format
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_smooth facet_wrap
#' @importFrom egg theme_article
#' @importFrom plotly ggplotly
#'
#' @return an interactive plot of SHAP values in association with feature values
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
#' myplot <- ShapFeaturePlot(shap_Mean_long)
#' }
ShapFeaturePlot <- function(shap_Mean_long){
  # utils::globalVariables(c("f_val", "Phi", "correct_prediction"))
  f_val <- NULL
  Phi <- NULL
  correct_prediction <- NULL
  SHAP_vs_FVAL_plt <- ggplot(shap_Mean_long, aes(x = f_val, y = Phi)) +
    geom_point(aes(shape = correct_prediction), size = 1, alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ x) +
    ggplot2::xlab("normalized feature values [0 1]") +
    ggplot2::ylab("SHAP values") +
    facet_wrap(~ feature, scales = "free_y") +
    egg::theme_article() # base_size = 8

  SHAP_vs_FVAL_plt <- ggplotly(SHAP_vs_FVAL_plt)
  # display the plot
  return(SHAP_vs_FVAL_plt)
}
