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
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
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
  pred_results <- trained_model$predict(task,splits$test)
  # plot confusion matrix
  d_binomial <- tibble("Truth" = featset_total_test[,task$target_names],
                       "Prediction" = pred_results$response)
  basic_table <- table(d_binomial)
  cfm <- broom::tidy(basic_table)
  CM_plt_test <- cvms::plot_confusion_matrix(cfm,target_col = "Truth",
                                       prediction_col = "Prediction",
                                       counts_col = "n", palette = palette,
                                       # font_counts = font(size = 2),
                                       # font_normalized = font(size = 2),
                                       # font_row_percentages = font(size = 2),
                                       # font_col_percentages = font(size = 2),
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
  pred_results <- trained_model$predict(task,splits$train)
  # plot confusion matrix
  d_binomial <- tibble("Truth" = featset_total_train[,task$target_names],
                       "Prediction" = pred_results$response)
  basic_table <- table(d_binomial)
  cfm <- broom::tidy(basic_table)
  CM_plt_train <- cvms::plot_confusion_matrix(cfm,target_col = "Truth",
                                             prediction_col = "Prediction",
                                             counts_col = "n", palette = palette,
                                             # font_counts = font(size = 2),
                                             # font_normalized = font(size = 2),
                                             # font_row_percentages = font(size = 2),
                                             # font_col_percentages = font(size = 2),
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
                           labels = c("train set","test set"),
                           # label.args = list(gp = grid::gpar(font = 1, cex = 0.5)),
                           nrow = 1,
                           ncol=2)
  # list(CM_plt_test,CM_plt_train)
  return(CM_plt_both)
}
