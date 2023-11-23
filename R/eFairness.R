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
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
eFairness <- function(task,
                      trained_model,
                      splits,
                      target_variable,
                      var_levels) {

  # utils::globalVariables(c("truth", "pos_class_prob", "subgroups"))
  truth <- NULL
  pos_class_prob <- NULL
  subgroups <- NULL
  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  # plot the confusion matrix for the test set
  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)

  var_levels_codes <- levels(featset_total[,target_variable])
  meas = c("classif.auc",
           "classif.bacc",
           "classif.mcc",
           "classif.bbrier",
           "classif.ppv",
           "classif.npv",
           "classif.specificity",
           "classif.sensitivity",
           "classif.prauc") #mlr3


  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$train)
  pred_results <- as.data.table(pred_results)
  set(pred_results, j = "subgroups" , value = featset_total[,target_variable])
  pred_results$subgroups <- factor(pred_results$subgroups, levels = var_levels_codes,labels = var_levels)


  colnames(pred_results)[4] <- "pos_class_prob"

  # Plot ROC curves
  ROC_plt_dev <- ggplot(data = pred_results, aes(d = truth, m = pos_class_prob, color = subgroups)) +
    plotROC::geom_roc() +
    plotROC::style_roc()

  # for test set
  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)
  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$test)
  pred_results <- as.data.table(pred_results)
  var_levels_codes <- levels(featset_total_test[,target_variable])
  set(pred_results, j = "subgroups" , value = featset_total_test[,target_variable])
  pred_results$subgroups <- factor(pred_results$subgroups, levels = var_levels_codes,labels = var_levels)

  colnames(pred_results)[4] <- "pos_class_prob"
  # Plot ROC curves
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
  for (i in 1:length(var_levels_codes)){
    idx <- which(featset_total[,target_variable] == var_levels_codes[i])
    pred_results <- trained_model$predict(task,splits$train[idx])
    prf_dev <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))
    prf_dev$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_dev$`pred_results$score(measures = mlr3::msrs(meas))`,2)
    rownames(prf_dev) <- gsub("classif.","",rownames(prf_dev))
    prf_dev_list[[i]] <- prf_dev
    colnames(prf_dev_list[[i]])[1] <- var_levels[i]
  }
  # combine the lists into one data frame for performance metrics
  prf_dev_df <- as.data.frame(do.call(cbind, prf_dev_list))

  prf_test_list <- list()
  for (i in 1:length(var_levels_codes)){
    idx <- which(featset_total_test[,target_variable] == var_levels_codes[i])
    pred_results <- trained_model$predict(task,splits$test[idx])
    prf_test <- as.data.frame(pred_results$score(measures = mlr3::msrs(meas)))
    prf_test$`pred_results$score(measures = mlr3::msrs(meas))` <- round(prf_test$`pred_results$score(measures = mlr3::msrs(meas))`,2)
    rownames(prf_test) <- gsub("classif.","",rownames(prf_test))
    prf_test_list[[i]] <- prf_test
    colnames(prf_test_list[[i]])[1] <- var_levels[i]
  }

  # combine the lists into one data frame for performance metrics
  prf_test_df <- as.data.frame(do.call(cbind, prf_test_list))

  return(list(all_plts, prf_dev_df, prf_test_df))
}
