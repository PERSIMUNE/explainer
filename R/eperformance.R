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
#' # Set environment variables for reproducibility
#' Sys.setenv(LANG = "en") # Change R language to English!
#' RNGkind("L'Ecuyer-CMRG") # Change to L'Ecuyer-CMRG instead of the default "Mersenne-Twister"
#'
#' # Load required libraries
#' library("explainer")
#'
#' # Set seed for reproducibility
#' seed <- 246
#' set.seed(seed)
#'
#' # Load necessary packages
#' if (!requireNamespace("mlbench", quietly = TRUE)) stop("mlbench not installed.")
#' if (!requireNamespace("mlr3learners", quietly = TRUE)) stop("mlr3learners not installed.")
#' if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger not installed.")
#' # Load BreastCancer dataset
#' utils::data("BreastCancer", package = "mlbench")
#'
#' # Keep the target column as "Class"
#' target_col <- "Class"
#'
#' # Change the positive class to "malignant"
#' positive_class <- "malignant"
#'
#' # Keep only the predictor variables and outcome
#' mydata <- BreastCancer[, -1] # 1 is ID
#'
#' # Remove rows with missing values
#' mydata <- na.omit(mydata)
#'
#' # Create a vector of sex categories
#' sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
#'
#' # Create a vector of age categories
#' mydata$age <- as.numeric(sample(seq(18, 60), size = nrow(mydata), replace = TRUE))
#'
#' # Add a sex column to the mydata data frame (for fairness analysis)
#' mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
#'
#' # Create a classification task
#' maintask <- mlr3::TaskClassif$new(
#'   id = "my_classification_task",
#'   backend = mydata,
#'   target = target_col,
#'   positive = positive_class
#' )
#'
#' # Create a train-test split
#' set.seed(seed)
#' splits <- mlr3::partition(maintask)
#'
#' # Add a learner (machine learning model base)
#' # Here we use random forest for example (you can use any other available model)
#' mylrn <- mlr3::lrn("classif.ranger", predict_type = "prob")
#'
#' # Train the model
#' mylrn$train(maintask, splits$train)
#'
#' # Make predictions on new data
#' mylrn$predict(maintask, splits$test)
#' ePerformance(task = maintask, trained_model = mylrn, splits = splits)
ePerformance <- function(task,
                         trained_model,
                         splits) {
  fpr <- NULL
  tpr <- NULL
  threshold <- NULL
  x <- NULL
  y <- NULL
  ppv <- NULL

  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  featset_total <- mydata[splits$train,]
  featset_total <- as.data.frame(featset_total)

  pred_results <- trained_model$predict(task, splits$train)
  pred_results_df <- data.table::as.data.table(pred_results)
  pred_results_df <- as.data.frame(pred_results_df)
  truth <- featset_total[, task$target_names]
  predicted_labels_dev <- pred_results$response
  PosClass <- task$positive
  NegClass <- task$negative
  random_guess <- data.frame(x = c(0, 1), y = c(0, 1))

  calprd <- c()
  TPR_dev <- c()
  FPR_dev <- c()
  PPV_dev <- c()
  th_step <- 1

  for (th in seq(0, 1.01, .01)) {
    for (i in 1:length(truth)) {
      if (pred_results_df[i, paste0("prob.", PosClass)] >= th) {
        calprd[i] <- PosClass
      } else {
        calprd[i] <- NegClass
      }
    }
    TPR_dev[th_step] <- sum((calprd == PosClass & truth == PosClass) == T) / sum(truth == PosClass)
    FPR_dev[th_step] <- sum((calprd == PosClass & truth == NegClass) == T) / sum(truth == NegClass)
    TP <- sum(calprd == PosClass & truth == PosClass)
    FP <- sum(calprd == PosClass & truth == NegClass)
    PPV_dev[th_step] <- TP / (TP + FP)
    th_step <- th_step + 1
  }

  dev_roc_res <- data.frame(tpr = TPR_dev,
                            fpr = FPR_dev,
                            threshold = seq(0, 1.01, .01))
  dev_roc_res <- dev_roc_res[complete.cases(dev_roc_res),]

  dev_roc_plt <- ggplot(data = dev_roc_res, mapping = aes(fpr, tpr)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    geom_line(size = 0.2, linetype = "dashed", colour = 'blue') +
    scale_colour_gradient2(low = 'blue', high = 'red', mid = "green", midpoint = 0.5) +
    geom_line(data = random_guess, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title = paste0("development set, N=", nrow(featset_total)),
         x = "False Positive Rate (FPR)",
         y = "True Positive Rate (TPR)") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  dev_roc_plt <- dev_roc_plt + theme(legend.position = c(0.8, 0.25))
  dev_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  Pnum <- length(which(truth == PosClass))
  Tnum <- nrow(featset_total)
  PRcurve.baseline <- data.frame(x = c(0, 1), y = c(Pnum / Tnum, Pnum / Tnum))

  dev_PRroc_res <- data.frame(tpr = TPR_dev,
                              ppv = PPV_dev,
                              threshold = seq(0, 1.01, .01))
  dev_PRroc_res <- dev_PRroc_res[complete.cases(dev_PRroc_res),]
  dev_PRroc_plt <- ggplot(data = dev_PRroc_res, mapping = aes(tpr, ppv)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    scale_colour_gradient2(low = 'blue', high = 'red', mid = "green", midpoint = 0.5) +
    geom_line(size = 0.2, linetype = "dashed", colour = 'blue') +
    geom_line(data = PRcurve.baseline, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title = paste0("development set, N=", nrow(featset_total)),
         x = "Recall",
         y = "Precision") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  dev_PRroc_plt <- dev_PRroc_plt + theme(legend.position = c(0.2, 0.25))
  dev_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  test_set <- mydata[splits$test,]
  test_set <- as.data.frame(test_set)

  pred_results <- trained_model$predict(task, splits$test)
  pred_results_df <- data.table::as.data.table(pred_results)
  pred_results_df <- as.data.frame(pred_results_df)
  truth <- test_set[, task$target_names]
  predicted_labels_test <- pred_results$response
  PosClass <- task$positive
  NegClass <- task$negative
  random_guess <- data.frame(x = c(0, 1), y = c(0, 1))

  calprd <- c()
  TPR_test <- c()
  FPR_test <- c()
  PPV_test <- c()
  th_step <- 1

  for (th in seq(0, 1.01, .01)) {
    for (i in 1:length(truth)) {
      if (pred_results_df[i, paste0("prob.", PosClass)] >= th) {
        calprd[i] <- PosClass
      } else {
        calprd[i] <- NegClass
      }
    }
    TPR_test[th_step] = sum((calprd == PosClass & truth == PosClass) == T) / sum(truth == PosClass)
    FPR_test[th_step] = sum((calprd == PosClass & truth == NegClass) == T) / sum(truth == NegClass)
    TP <- sum(calprd == PosClass & truth == PosClass)
    FP <- sum(calprd == PosClass & truth == NegClass)
    PPV_test[th_step] <- TP / (TP + FP)
    th_step <- th_step + 1
  }

  test_roc_res <- data.frame(tpr = TPR_test,
                             fpr = FPR_test,
                             threshold = seq(0, 1.01, .01))
  test_roc_res <- test_roc_res[complete.cases(test_roc_res),]

  test_roc_plt <- ggplot(data = test_roc_res, mapping = aes(fpr, tpr)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    geom_line(size = 0.2, linetype = "dashed", colour = 'blue') +
    scale_colour_gradient2(low = 'blue', high = 'red', mid = "green", midpoint = 0.5) +
    geom_line(data = random_guess, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title = paste0("test set, N=", nrow(test_set)),
         x = "False Positive Rate (FPR)",
         y = "True Positive Rate (TPR)") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  test_roc_plt <- test_roc_plt + theme(legend.position = c(0.8, 0.25))
  test_roc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  Pnum <- length(which(truth == PosClass))
  Tnum <- nrow(test_set)
  PRcurve.baseline <- data.frame(x = c(0, 1), y = c(Pnum / Tnum, Pnum / Tnum))

  test_PRroc_res <- data.frame(tpr = TPR_test,
                               ppv = PPV_test,
                               threshold = seq(0, 1.01, .01))
  test_PRroc_res <- test_PRroc_res[complete.cases(test_PRroc_res),]
  test_PRroc_plt <- ggplot(data = test_PRroc_res, mapping = aes(tpr, ppv)) +
    geom_point(size = 0.5, aes(colour = threshold)) +
    scale_colour_gradient2(low = 'blue', high = 'red', mid = "green", midpoint = 0.5) +
    geom_line(size = 0.2, linetype = "dashed", colour = 'blue') +
    geom_line(data = PRcurve.baseline, aes(x = x, y = y), color = "grey", linetype = "dashed") +
    labs(title = paste0("test set, N=", nrow(test_set)),
         x = "Recall",
         y = "Precision") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5), labels = seq(0, 1, 0.5)) +
    egg::theme_article()
  test_PRroc_plt <- test_PRroc_plt + theme(legend.position = c(0.2, 0.25))
  test_PRroc_plt[["theme"]][["plot.title"]][["size"]] <- 10

  all_plts <- ggpubr::ggarrange(plotlist = list(dev_PRroc_plt, dev_roc_plt, test_PRroc_plt, test_roc_plt),
                                ncol = 2, nrow = 2,
                                common.legend = TRUE,
                                legend = "right")

  dev_PRroc_plt <- dev_PRroc_plt + theme(legend.position = "none")
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

  test_PRroc_plt <- test_PRroc_plt + theme(legend.position = "none")
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

  return(list(all_plts, fig_dev, fig_test))
}
