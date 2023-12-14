#' @title Enhanced ROC and Precision-Recall Plots
#'
#' @description
#' This function generates Precision-Recall and ROC curves for binary classification models.
#'
#' @param task mlr3 binary classification task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#'
#' @importFrom ggplot2 labs theme element_blank guides
#' @export
#'
#' @return ROC and Precision-Recall curves
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
#' myplot <- eROC_plot(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits
#' )
eROC_plot <- function(task,
                      trained_model,
                      splits) {

  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)

  pred_results <- trained_model$predict(task, splits$train)

  meas <- c("classif.auc",
            "classif.bacc",
            "classif.mcc",
            "classif.bbrier",
            "classif.ppv",
            "classif.npv",
            "classif.specificity",
            "classif.sensitivity",
            "classif.prauc")

  prauc_idx <- which(meas == "classif.prauc")
  auc_idx <- which(meas == "classif.auc")

  prf_dev <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))

  bmr_roc_plt <- mlr3viz::autoplot(pred_results, type = "roc")
  bmr_PRroc_plt <- mlr3viz::autoplot(pred_results, type = "prc")

  bmr_PRroc_plt <- bmr_PRroc_plt +
    labs(title = paste0("development set N=", nrow(featset_total), ", PRAUC = ", round(prf_dev[prauc_idx, 1], 2)),
         x = "Recall", y = "Precision") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.9), legend.title = element_blank()) +
    guides(color = "none")
  bmr_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_PRroc_plt[["theme"]][["plot.subtitle"]][["size"]] <- 10

  bmr_roc_plt <- bmr_roc_plt +
    labs(title = paste0("development set N=", nrow(featset_total), ", AUC = ", round(prf_dev[auc_idx, 1], 2)),
         x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.1), legend.title = element_blank()) +
    guides(color = "none")
  bmr_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_roc_plt[["theme"]][["plot.subtitle"]][["size"]] <- 10

  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)

  pred_results_test <- trained_model$predict(task, splits$test)
  prf_test <- as.data.frame(pred_results_test$score(measures = mlr3::msrs(meas)))

  bmr_roc_plt_test <- mlr3viz::autoplot(pred_results_test, type = "roc")
  bmr_PRroc_plt_test <- mlr3viz::autoplot(pred_results_test, type = "prc")

  bmr_PRroc_plt_test <- bmr_PRroc_plt_test +
    labs(title = paste0("test set N=", nrow(featset_total_test), ", PRAUC = ", round(prf_test[auc_idx, 1], 2)),
         x = "Recall", y = "Precision") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.9), legend.title = element_blank()) +
    guides(color = "none")
  bmr_PRroc_plt_test[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_PRroc_plt_test[["theme"]][["plot.subtitle"]][["size"]] <- 10

  bmr_roc_plt_test <- bmr_roc_plt_test +
    labs(title = paste0("test set N=", nrow(featset_total_test), ", AUC = ", round(prf_test[prauc_idx, 1], 2)),
         x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.1), legend.title = element_blank()) +
    guides(color = "none")
  bmr_roc_plt_test[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_roc_plt_test[["theme"]][["plot.subtitle"]][["size"]] <- 10

  all_plts <- ggpubr::ggarrange(plotlist = list(bmr_roc_plt, bmr_PRroc_plt, bmr_roc_plt_test, bmr_PRroc_plt_test))

  prf_test$`pred_results_test$score(measures = mlr3::msrs(meas))` <- round(prf_test$`pred_results_test$score(measures = mlr3::msrs(meas))`, 2)
  prf_dev$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_dev$`pred_results$score(measures = mlr3::msrs(meas))`, 2)
  rownames(prf_test) <- gsub("classif.", "", rownames(prf_test))
  rownames(prf_dev) <- gsub("classif.", "", rownames(prf_dev))

  return(list(all_plts, prf_dev, prf_test))
}
