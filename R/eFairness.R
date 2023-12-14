#' @title Enhanced Fairness Analysis
#'
#' @description
#' This function generates Precision-Recall and ROC curves for sample subgroups, facilitating fairness analysis of a binary classification model.
#'
#' @param task mlr3 binary classification task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#' @param target_variable character, the variable from the dataset used to test the model's performance against
#' @param var_levels list, defining the levels for the specified variable
#'
#' @importFrom data.table as.data.table set
#' @importFrom ggplot2 ggplot aes labs
#' @export
#'
#' @return Model performance metrics for user-specified subgroups using Precision-Recall and ROC curves
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
#' # sex is chosen for fairness analysis
#' Fairness_results <- eFairness(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   target_variable = "sex",
#'   var_levels = c("Male", "Female")
#' )
eFairness <- function(task,
                      trained_model,
                      splits,
                      target_variable,
                      var_levels) {

  truth <- NULL
  pos_class_prob <- NULL
  subgroups <- NULL
  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)

  var_levels_codes <- levels(featset_total[, target_variable])
  meas <- c("classif.auc",
            "classif.bacc",
            "classif.mcc",
            "classif.bbrier",
            "classif.ppv",
            "classif.npv",
            "classif.specificity",
            "classif.sensitivity",
            "classif.prauc") #mlr3

  pred_results <- trained_model$predict(task, splits$train)
  pred_results <- as.data.table(pred_results)
  set(pred_results, j = "subgroups", value = featset_total[, target_variable])
  pred_results$subgroups <- factor(pred_results$subgroups, levels = var_levels_codes, labels = var_levels)

  colnames(pred_results)[4] <- "pos_class_prob"

  ROC_plt_dev <- ggplot(data = pred_results, aes(d = truth, m = pos_class_prob, color = subgroups)) +
    plotROC::geom_roc() +
    plotROC::style_roc()

  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)

  pred_results <- trained_model$predict(task, splits$test)
  pred_results <- as.data.table(pred_results)
  var_levels_codes <- levels(featset_total_test[, target_variable])
  set(pred_results, j = "subgroups", value = featset_total_test[, target_variable])
  pred_results$subgroups <- factor(pred_results$subgroups, levels = var_levels_codes, labels = var_levels)

  colnames(pred_results)[4] <- "pos_class_prob"

  ROC_plt_test <- ggplot(data = pred_results, aes(d = truth, m = pos_class_prob, color = subgroups)) +
    plotROC::geom_roc() +
    plotROC::style_roc()

  ROC_plt_dev <- ROC_plt_dev + labs(title = "ROC plot for development Set")
  ROC_plt_test <- ROC_plt_test + labs(title = "ROC plot for test Set")
  all_plts <- ggpubr::ggarrange(plotlist = list(ROC_plt_dev, ROC_plt_test),
                                ncol = 2, nrow = 1,
                                common.legend = TRUE,
                                legend = "right")

  prf_dev_list <- list()
  for (i in 1:length(var_levels_codes)) {
    idx <- which(featset_total[, target_variable] == var_levels_codes[i])
    pred_results <- trained_model$predict(task, splits$train[idx])
    prf_dev <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))
    prf_dev$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_dev$`pred_results$score(measures = mlr3::msrs(meas))`, 2)
    rownames(prf_dev) <- gsub("classif.", "", rownames(prf_dev))
    prf_dev_list[[i]] <- prf_dev
    colnames(prf_dev_list[[i]])[1] <- var_levels[i]
  }

  prf_dev_df <- as.data.frame(do.call(cbind, prf_dev_list))

  prf_test_list <- list()
  for (i in 1:length(var_levels_codes)) {
    idx <- which(featset_total_test[, target_variable] == var_levels_codes[i])
    pred_results <- trained_model$predict(task, splits$test[idx])
    prf_test <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))
    prf_test$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_test$`pred_results$score(measures = mlr3::msrs(meas))`, 2)
    rownames(prf_test) <- gsub("classif.", "", rownames(prf_test))
    prf_test_list[[i]] <- prf_test
    colnames(prf_test_list[[i]])[1] <- var_levels[i]
  }

  prf_test_df <- as.data.frame(do.call(cbind, prf_test_list))

  return(list(all_plts, prf_dev_df, prf_test_df))
}
