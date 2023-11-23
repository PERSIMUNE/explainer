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
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
eROC_plot <- function(task,
                      trained_model,
                      splits) {

  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  # plot the confusion matrix for the test set
  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)
  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$train)

  meas = c("classif.auc",
           "classif.bacc",
           "classif.mcc",
           "classif.bbrier",
           "classif.ppv",
           "classif.npv",
           "classif.specificity",
           "classif.sensitivity",
           "classif.prauc") #mlr3
  prauc_idx <- which(meas=="classif.prauc")
  auc_idx <- which(meas=="classif.auc")
  prf_dev <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))

  bmr_roc_plt <- mlr3viz::autoplot(pred_results, type = "roc")
  bmr_PRroc_plt <- mlr3viz::autoplot(pred_results, type = "prc")
  # bmr_PRroc = mlr::generateThreshVsPerfData(bmr, measures = list(ppv, tpr)) #mlr corrections
  # Precision-recall curve
  # paste0("PRAUC = ", round(prf_dev[["classif.prauc"]], 2))
  # print(prf_dev)

  bmr_PRroc_plt <- bmr_PRroc_plt +
    labs(title= paste0("development set N=",nrow(featset_total), ", PRAUC = ", round(prf_dev[prauc_idx,1],2)),
         x = "Recall", y = "Precision") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.9),legend.title = element_blank())+
    guides(color = "none")
  bmr_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_PRroc_plt[["theme"]][["plot.subtitle"]][["size"]] <- 10

  bmr_roc_plt <- bmr_roc_plt +
    labs(title= paste0("development set N=",nrow(featset_total), ", AUC = ", round(prf_dev[auc_idx,1],2)),
         x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.1),legend.title = element_blank())+
    guides(color = "none")
  bmr_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_roc_plt[["theme"]][["plot.subtitle"]][["size"]] <- 10

  # for test set
  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)
  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$test)

  bmr_roc_plt_test <- mlr3viz::autoplot(pred_results, type = "roc")
  bmr_PRroc_plt_test <- mlr3viz::autoplot(pred_results, type = "prc")

  pred_results_test <- trained_model$predict(task,splits$test)
  prf_test <- as.data.frame(pred_results_test$score(measures = mlr3::msrs(meas)))
  # Precision-recall curve
  bmr_PRroc_plt_test <- bmr_PRroc_plt_test +
    labs(title= paste0("test set N=",nrow(featset_total_test), ", PRAUC = ", round(prf_test[auc_idx,1],2)),
         x = "Recall", y = "Precision") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.9),legend.title = element_blank())+
    guides(color = "none")
  bmr_PRroc_plt_test[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_PRroc_plt_test[["theme"]][["plot.subtitle"]][["size"]] <- 10

  bmr_roc_plt_test <- bmr_roc_plt_test +
    labs(title= paste0("test set N=",nrow(featset_total_test), ", AUC = ", round(prf_test[prauc_idx,1],2)),
         x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    egg::theme_article() +
    theme(legend.position = c(0.8, 0.1),legend.title = element_blank())+
    guides(color = "none")
  bmr_roc_plt_test[["theme"]][["plot.title"]][["size"]] <- 10
  bmr_roc_plt_test[["theme"]][["plot.subtitle"]][["size"]] <- 10

  all_plts <- ggpubr::ggarrange(plotlist = list(bmr_roc_plt, bmr_PRroc_plt, bmr_roc_plt_test,bmr_PRroc_plt_test))

  prf_test$`pred_results_test$score(measures = mlr3::msrs(meas))` <- round(prf_test$`pred_results_test$score(measures = mlr3::msrs(meas))`,2)
  prf_dev$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_dev$`pred_results$score(measures = mlr3::msrs(meas))`,2)
  rownames(prf_test) <- gsub("classif.","",rownames(prf_test))
  rownames(prf_dev) <- gsub("classif.","",rownames(prf_dev))

  return(list(all_plts, prf_dev, prf_test))
}
