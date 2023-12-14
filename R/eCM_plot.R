#' @title Enhanced Confusion Matrix Plot
#' @description This function generates an enhanced confusion matrix plot using the CVMS package. The plot includes visualizations of sensitivity, specificity, positive predictive value (PPV), and negative predictive value (NPV).
#'
#' @param task mlr3 task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#' @param add_sums logical, indicating whether total numbers should be displayed in the plot (default: TRUE)
#' @param palette character, the color palette for the confusion matrix (default: "Green")
#'
#' @return A confusion matrix plot visualizing sensitivity, specificity, PPV, and NPV
#' @export
#'
#' @examples
#' library("explainer")
#' seed <- 246
#' set.seed(seed)
#'
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
#' myplot <- eCM_plot(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits
#' )
eCM_plot <- function(task,
                     trained_model,
                     splits,
                     add_sums = TRUE,
                     palette = "Green") {
  # library(ggplot2)
  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  # plot the confusion matrix for the test set
  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)
  pred_results <- trained_model$predict(task, splits$test)
  # plot confusion matrix
  d_binomial <- tibble("Truth" = featset_total_test[, task$target_names],
                       "Prediction" = pred_results$response)
  basic_table <- table(d_binomial)
  cfm <- tibble::as_tibble(basic_table)
  CM_plt_test <- cvms::plot_confusion_matrix(cfm, target_col = "Truth",
                                             prediction_col = "Prediction",
                                             counts_col = "n", palette = palette,
                                             theme_fn = ggplot2::theme_minimal,
                                             add_sums = add_sums,
                                             sums_settings = cvms::sum_tile_settings(
                                               palette = "Oranges",
                                               label = "Total",
                                               tc_tile_border_color = "black"
                                             ))
  CM_plt_test[["labels"]][["x"]] <- 'Truth (observation)'
  CM_plt_test[["labels"]][["y"]] <- 'Prediction (model output)'
  CM_plt_test[["theme"]][["text"]][["size"]] <- 9
  CM_plt_test[["theme"]][["axis.text"]][["size"]] <- 9
  CM_plt_test[["theme"]][["text"]][["family"]] <- 'Helvetica'

  # plot the confusion matrix for the train set
  featset_total_train <- mydata[splits$train,]
  featset_total_train <- as.data.frame(featset_total_train)
  pred_results <- trained_model$predict(task, splits$train)
  d_binomial <- tibble("Truth" = featset_total_train[, task$target_names],
                       "Prediction" = pred_results$response)
  basic_table <- table(d_binomial)
  # cfm <- broom::tidy(basic_table)
  cfm <- tibble::as_tibble(basic_table)
  CM_plt_train <- cvms::plot_confusion_matrix(cfm, target_col = "Truth",
                                              prediction_col = "Prediction",
                                              counts_col = "n", palette = palette,
                                              theme_fn = ggplot2::theme_minimal,
                                              add_sums = add_sums,
                                              sums_settings = cvms::sum_tile_settings(
                                                palette = "Oranges",
                                                label = "Total",
                                                tc_tile_border_color = "black"
                                              ))
  CM_plt_train[["labels"]][["x"]] <- 'Truth (observation)'
  CM_plt_train[["labels"]][["y"]] <- 'Prediction (model output)'
  CM_plt_train[["theme"]][["text"]][["size"]] <- 9
  CM_plt_train[["theme"]][["axis.text"]][["size"]] <- 9
  CM_plt_train[["theme"]][["text"]][["family"]] <- 'Helvetica'

  CM_plt_both <- egg::ggarrange(CM_plt_train,
                                CM_plt_test,
                                labels = c("train set", "test set"),
                                nrow = 1,
                                ncol = 2)
  return(CM_plt_both)
}
