#' @title Enhanced Performance Evaluation
#'
#' @description
#' This function generates Precision-Recall and ROC curves, including threshold information for binary classification models.
#'
#' @param task mlr3 binary classification task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_colour_gradient2 labs scale_x_continuous scale_y_continuous
#' @importFrom plotly ggplotly subplot layout
#' @importFrom magrittr %>%
#' @importFrom stats complete.cases
#' @importFrom tidyr gather
#' @export
#'
#' @return ROC and Precision-Recall curves with threshold information
#'
#' @examples
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }

eperformance <- function(task,
                         trained_model,
                         splits) {
  # utils::globalVariables(c("fpr", "tpr", "threshold", "x", "y", "scale_x_continuous", "scale_y_continuous", "ppv"))
  fpr <- NULL
  tpr <- NULL
  threshold <- NULL
  x <- NULL
  y <- NULL
  ppv <- NULL
  # library(ggplot2)
  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  # plot the confusion matrix for the dev set
  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)
  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$train)
  pred_results_df <- data.table::as.data.table(pred_results)
  pred_results_df <- as.data.frame(pred_results_df)
  truth <- featset_total[,task$target_names]
  predicted_labels_dev <- pred_results$response
  PosClass <- task$positive
  NegClass <- task$negative
  random_guess <- data.frame(x=c(0,1),y=c(0,1))

  # compute AUC with calibration
  calprd <- c()
  TPR_dev <- c()
  FPR_dev <- c()
  PPV_dev <- c()
  th_step = 1

  for (th in seq(0,1.01,.01)){
    for (i in 1:length(truth)){
      if (pred_results_df[i,paste0("prob.",PosClass)]>=th){
        calprd[i] <- PosClass
      } else {
        calprd[i] <- NegClass
      }
    }
    TPR_dev[th_step] = sum((calprd==PosClass & truth==PosClass)==T)/sum(truth==PosClass)
    FPR_dev[th_step] = sum((calprd==PosClass & truth==NegClass)==T)/sum(truth==NegClass)
    TP <- sum(calprd==PosClass & truth==PosClass)
    FP <- sum(calprd==PosClass & truth==NegClass)
    (PPV_dev[th_step] <- TP/(TP+FP))
    th_step = th_step + 1
  }

  dev_roc_res <- data.frame(tpr = TPR_dev,
                             fpr=FPR_dev,
                             threshold = seq(0,1.01,.01))
  dev_roc_res <- dev_roc_res[complete.cases(dev_roc_res),]

  dev_roc_plt <- ggplot(data = dev_roc_res, mapping = aes(fpr,tpr)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    geom_line(size = 0.2, linetype = "dashed", colour='blue') +
    scale_colour_gradient2(low = 'blue', high = 'red',mid = "green",midpoint = 0.5) +
    geom_line(data = random_guess, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title= paste0("development set, N=",nrow(featset_total)),
         x = "False Positive Rate (FPR)",
         y = "True Positive Rate (TPR)") +
    scale_x_continuous(limits = c(0,1),breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  dev_roc_plt <- dev_roc_plt + theme(legend.position = c(0.8, 0.25))
  dev_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10

    # Precision-recall curve dev set
  Pnum <- length(which(truth==PosClass)) # number of positive cases in dev set
  Tnum <- nrow(featset_total) # number of total cases in dev set
  PRcurve.baseline <- data.frame(x=c(0,1),y=c(Pnum/Tnum,Pnum/Tnum)) # baseline for the PR curve

  dev_PRroc_res <- data.frame(tpr = TPR_dev,
                               ppv=PPV_dev,
                               threshold = seq(0,1.01,.01))
  dev_PRroc_res <- dev_PRroc_res[complete.cases(dev_PRroc_res),]
  dev_PRroc_plt <- ggplot(data = dev_PRroc_res, mapping = aes(tpr,ppv))+
    geom_point(size = 0.5, aes(colour = threshold)) +
    scale_colour_gradient2(low = 'blue', high = 'red',mid = "green",midpoint = 0.5) +
    geom_line(size = 0.2,linetype = "dashed",colour='blue') +
    geom_line(data = PRcurve.baseline, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title= paste0("development set, N=",nrow(featset_total)),
         x = "Recall",
         y = "Precision") +
    scale_x_continuous(limits = c(0,1),breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  dev_PRroc_plt <- dev_PRroc_plt + theme(legend.position = c(0.2, 0.25))
  dev_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  ###############################################################
  # plot the confusion matrix for the dev set
  test_set <- mydata[splits$test,]
  test_set <- as.data.frame(test_set)
  # extract prediction results from mlr3 object and convert it to data.table as it is not a data.table
  pred_results <- trained_model$predict(task,splits$test)
  pred_results_df <- data.table::as.data.table(pred_results)
  pred_results_df <- as.data.frame(pred_results_df)
  truth <- test_set[,task$target_names]
  predicted_labels_test <- pred_results$response
  PosClass <- task$positive
  NegClass <- task$negative
  random_guess <- data.frame(x=c(0,1),y=c(0,1))

  # compute AUC with calibration
  calprd <- c()
  TPR_test <- c()
  FPR_test <- c()
  PPV_test <- c()
  th_step = 1

  for (th in seq(0,1.01,.01)){
    for (i in 1:length(truth)){
      if (pred_results_df[i,paste0("prob.",PosClass)]>=th){
        calprd[i] <- PosClass
      } else {
        calprd[i] <- NegClass
      }
    }
    TPR_test[th_step] = sum((calprd==PosClass & truth==PosClass)==T)/sum(truth==PosClass)
    FPR_test[th_step] = sum((calprd==PosClass & truth==NegClass)==T)/sum(truth==NegClass)
    TP <- sum(calprd==PosClass & truth==PosClass)
    FP <- sum(calprd==PosClass & truth==NegClass)
    (PPV_test[th_step] <- TP/(TP+FP))
    th_step = th_step + 1
  }

    test_roc_res <- data.frame(tpr = TPR_test,
                            fpr=FPR_test,
                            threshold = seq(0,1.01,.01))
    test_roc_res <- test_roc_res[complete.cases(test_roc_res),]
  test_roc_plt <- ggplot(data = test_roc_res, mapping = aes(fpr,tpr)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    geom_line(size = 0.2, linetype = "dashed", colour='blue') +
    scale_colour_gradient2(low = 'blue', high = 'red',mid = "green",midpoint = 0.5) +
    geom_line(data = random_guess, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title= paste0("test set, N=",nrow(test_set)),
         x = "False Positive Rate (FPR)",
         y = "True Positive Rate (TPR)") +
    scale_x_continuous(limits = c(0,1),breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  test_roc_plt <- test_roc_plt + theme(legend.position = c(0.8, 0.25))
  test_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10

    # Precision-recall curve dev set
  Pnum <- length(which(truth==PosClass)) # number of positive cases in dev set
  Tnum <- nrow(test_set) # number of total cases in dev set
  PRcurve.baseline <- data.frame(x=c(0,1),y=c(Pnum/Tnum,Pnum/Tnum)) # baseline for the PR curve

  test_PRroc_res <- data.frame(tpr = TPR_test,
                              ppv=PPV_test,
                              threshold = seq(0,1.01,.01))
  test_PRroc_res <- test_PRroc_res[complete.cases(test_PRroc_res),]
  test_PRroc_plt <- ggplot(data = test_PRroc_res, mapping = aes(tpr,ppv))+
    geom_point(size = 0.5, aes(colour = threshold)) +
    scale_colour_gradient2(low = 'blue', high = 'red',mid = "green",midpoint = 0.5) +
    geom_line(size = 0.2,linetype = "dashed",colour='blue') +
    geom_line(data = PRcurve.baseline, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title= paste0("test set, N=",nrow(test_set)),
         x = "Recall",
         y = "Precision") +
    scale_x_continuous(limits = c(0,1),breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.5),labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  test_PRroc_plt <- test_PRroc_plt + theme(legend.position = c(0.2, 0.25))
  test_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  all_plts <- ggpubr::ggarrange(plotlist = list(dev_PRroc_plt,dev_roc_plt,test_PRroc_plt,test_roc_plt),
                                ncol = 2, nrow = 2,
                                common.legend = TRUE,
                                legend = "right")

  # remove legend
  dev_PRroc_plt <- dev_PRroc_plt + theme(legend.position="none")
  dev_PRroc_plt <- ggplotly(dev_PRroc_plt)
  dev_roc_plt <- ggplotly(dev_roc_plt)
  fig_dev <- subplot(test_PRroc_plt, test_roc_plt) %>%
    layout(
      annotations = list(
        text = 'ROC plots on the development set',
        x = 0.5,
        y = 1,
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 10)
      )
    )

  # fig_dev <- subplot(test_PRroc_plt, test_roc_plt) %>%
  #   layout(title = 'ROC plots on the development set')

  # remove legend
  test_PRroc_plt <- test_PRroc_plt + theme(legend.position="none")
  test_PRroc_plt <- ggplotly(test_PRroc_plt)
  test_roc_plt <- ggplotly(test_roc_plt)
  fig_test <- subplot(test_PRroc_plt, test_roc_plt) %>%
    layout(
      annotations = list(
        text = 'ROC plots on the test set',
        x = 0.5,
        y = 1,
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 10)
      )
    )

  # fig_test <- subplot(test_PRroc_plt, test_roc_plt) %>%
  #   layout(title = 'ROC plots on the test set')

  return(list(all_plts,fig_dev,fig_test))
}
