#' @title SHAP Values versus Feature Values
#' @description SHAP values in association with feature values
#'
#' @param shap_Mean_long the data frame containing SHAP values in long format
#'
#' @importFrom ggplot2 ggplot aes geom_point stat_smooth facet_wrap
#' @importFrom egg theme_article
#' @importFrom plotly ggplotly
#'
#' @return interactive plot of SHAP values in association with feature values
#' @export
#'
#' @examples
#' \donttest{
#' library("explainer")
#' seed <- 246
#' set.seed(seed)
#' data("BreastCancer", package = "mlbench")
#' target_col <- "Class"
#' positive_class <- "malignant"
#' mydata <- BreastCancer[, -1]
#' mydata <- na.omit(mydata)
#' sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
#' mydata$age <- as.numeric(sample(seq(18,60), size = nrow(mydata), replace = TRUE))
#' mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
#' maintask <- mlr3::TaskClassif$new(id = "my_classification_task",backend = mydata,target = target_col,positive = positive_class)
#' splits <- mlr3::partition(maintask)
#' library("mlr3extralearners")
#' mylrn <- mlr3::lrn("classif.randomForest", predict_type = "prob")
#' mylrn$train(maintask, splits$train)
#' SHAP_output <- eSHAP_plot(task = maintask, trained_model = mylrn, splits = splits, sample.size = 30, seed = seed, subset = 0.8)
#' shap_Mean_long <- SHAP_output[[3]]
#' myplot <- ShapFeaturePlot(shap_Mean_long)
#' }
ShapFeaturePlot <- function(shap_Mean_long) {
  SHAP_vs_FVAL_plt <- ggplot(shap_Mean_long, aes(x = f_val, y = Phi)) +
    geom_point(aes(shape = correct_prediction), size = 1, alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ x) +
    xlab("normalized feature values [0 1]") +
    ylab("SHAP values") +
    facet_wrap(~ feature, scales = "free_y") +
    egg::theme_article()

  SHAP_vs_FVAL_plt <- ggplotly(SHAP_vs_FVAL_plt)
  return(SHAP_vs_FVAL_plt)
}
