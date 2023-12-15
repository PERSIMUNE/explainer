#' @title Enhanced SHAP Analysis for Binary Classification Models
#'
#' @description
#' The SHAP plot for classification models is a visualization tool that uses the Shapley value, an approach from cooperative game theory, to compute feature contributions for single predictions. The Shapley value fairly distributes the difference of the instance’s prediction and the datasets average prediction among the features. This method is available from the iml package.
#'
#' @param sample.size numeric, default to 30. The larger the value, the slower but more accurate the estimate of SHAP values
#' @param seed numeric, an integer for reproducibility. Default to 246
#' @param task mlr3 task object for binary classification
#' @param trained_model mlr3 trained learner object
#' @param splits mlr3 object defining data splits for train and test sets
#' @param subset numeric, what percentage of the instances to use from 0 to 1 where 1 means all
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_line coord_flip geom_jitter position_jitter scale_shape_manual labs scale_colour_gradient2 geom_text theme element_blank geom_hline element_text element_line ylim
#' @export
#'
#' @return
#' A list containing:
#' \item{shap_plot}{An enhanced SHAP plot with user interactive elements.}
#' \item{shap_Mean_wide}{A matrix of SHAP values.}
#' \item{shap_Mean}{A data.table with aggregated SHAP values.}
#' \item{shap}{Raw SHAP values.}
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
#' sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
#' mydata$age <- as.numeric(sample(seq(18,60), size = nrow(mydata), replace = TRUE))
#' mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
#' maintask <- mlr3::TaskClassif$new(
#'   id = "my_classification_task",
#'   backend = mydata,
#'   target = target_col,
#'   positive = positive_class
#' )
#' splits <- mlr3::partition(maintask)
#' mylrn <- mlr3::lrn("classif.ranger", predict_type = "prob")
#' mylrn$train(maintask, splits$train)
#' SHAP_output <- eSHAP_plot(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   sample.size = 2, # also 30 or more
#'   seed = seed,
#'   subset = 0.02 # up to 1
#' )
#' }
#'
#' @references
#' Molnar C, Casalicchio G, Bischl B. iml: An R package for interpretable machine learning. Journal of Open Source Software. 2018 Jun 27;3(26):786.
#'
#' @seealso
#' [eSHAP_plot_reg()]
#'
#' @keywords internal
#' @family classification
#' @family SHAP
eSHAP_plot <- function(task,
                       trained_model,
                       splits,
                       sample.size = 30,
                       seed = 246,
                       subset = 1) {

  # utils::globalVariables(c("feature", "sample_num", "correct_prediction"))
  feature <- NULL
  sample_num <- NULL
  correct_prediction <- NULL
  # library(ggplot2)
  mydata <- task$data()
  mydata <- as.data.frame(mydata)
  X <- mydata[which(names(mydata[splits$train,]) != task$target_names)]
  model <- iml::Predictor$new(trained_model, data = X, y = mydata[, task$target_names])
  # randomly subset the target variable and the corresponding rows
  if (subset < 1) {
    set.seed(seed) # set seed for reproducibility
    n <- round(subset * length(splits$test))
    target_index <- sample(splits$test, size = n, replace = FALSE)
    mydata <- mydata[target_index, ]

    # do the prediction for the test set
    pred_results <- trained_model$predict(task,target_index)

  } else {
    mydata <- mydata[splits$test, ]

    # do the prediction for the test set
    pred_results <- trained_model$predict(task,splits$test)

  }

  # the test set based on the data split is used to calculate SHAP values
  test_set <- as.data.frame(mydata)
  feature_names <- colnames(X)
  nfeats <- length(feature_names)



  # save the predicted probability for the positive class (assuming with have a binary classification task)
  pred_prob <- pred_results$prob[,2]
  # mark which samples were correctly predicted and which samples were not
  predicted_correct <- mydata$Class==pred_results$response

  test_set.nolab <- mydata
  # initialize the results list.
  shap_values <- vector("list", nrow(test_set))
  for (i in seq_along(shap_values)) {
    set.seed(seed)
    shap_values[[i]] <- iml::Shapley$new(model, x.interest = test_set[i,feature_names],
                                         sample.size = sample.size)$results
    shap_values[[i]]$sample_num <- i  # identifier to track our instances.
    shap_values[[i]]$predcorrectness <- predicted_correct[i]
    shap_values[[i]]$pred_prob <- pred_prob[i]
  }
  data_shap_values <- dplyr::bind_rows(shap_values)  # collapse the list.

  shap <- data_shap_values[which(data_shap_values$class==task$positive),]

  total_reps <- nrow(shap)/nfeats
  mean_phi <- rep(0,nfeats)
  indiv_phi <- rep(0,nfeats)
  f_val_lst <- rep(0,nfeats)
  indiv_correctness <- rep(0,nfeats)
  pred_prob_rep <- rep(0,nfeats)

  feature_values <- gsub(".*=",'',shap$feature.value)
  shap$feature.value <- as.numeric(feature_values)
  for (i in 1:nfeats){
    mean_phi[i] = mean(abs(shap$phi[seq(i,nrow(shap),nfeats)[predicted_correct]]))# only correct predictions to calculate the means
    indiv_phi[i] = list(shap$phi[seq(i,nrow(shap),nfeats)])
    f_val_lst[i] = list(feature_values[seq(i,nrow(shap),nfeats)])
    indiv_correctness[i] = list(shap$predcorrectness[seq(i,nrow(shap),nfeats)])
    pred_prob_rep[i] = list(shap$pred_prob[seq(i,nrow(shap),nfeats)])
  }


  # test_set.nolab[,task$target_names:=NULL]
  test_set.nolab[,task$target_names] <- NULL
  # get the column names of the data frame
  cols <- colnames(test_set.nolab)

  # loop through each column
  for (col in cols) {
    # check if the column is numeric
    if (!is.numeric(test_set.nolab[[col]])) {
      # convert non-numeric columns to numeric
      test_set.nolab[[col]] <- as.numeric(test_set.nolab[[col]])
    }
  }

  # apply transformation for visualization
  for (i in 1:length(f_val_lst)){
    f_val_lst[[i]] <- range01(test_set.nolab[,i])
  }

  (f_val = as.numeric(unlist(f_val_lst)))
  (Phi = unlist(indiv_phi))

  shap_Mean <- data.table::data.table(feature=rep(feature_names,each=total_reps),
                                      mean_phi = rep(mean_phi,each=total_reps),
                                      Phi = Phi,
                                      f_val = f_val,
                                      sample_num = rep(1:nrow(test_set),length(feature_names)),
                                      correct_prediction = unlist(indiv_correctness),
                                      pred_prob = unlist(pred_prob_rep))

  shap_Mean_wide <- data.table::dcast(shap_Mean, sample_num ~ feature, value.var="Phi")

  shap_Mean$correct_prediction <- factor(shap_Mean$correct_prediction, levels = c(FALSE, TRUE), labels = c("Incorrect","Correct"))
  shap_plot <- shap_Mean %>%
    mutate(feature = forcats::fct_reorder(feature, mean_phi)) %>%
    ggplot(aes(x = feature, y = Phi, color = f_val))+
    geom_violin(colour = "grey") +
    geom_line(aes(group = sample_num), alpha = 0.1,size=0.2) +
    coord_flip() +
    geom_jitter(alpha = 0.6,size=1, position=position_jitter(width=0.2, height=0),aes(shape=correct_prediction)) +
    scale_shape_manual(values=c(4, 19))+
    # scale_color_manual(values=c("black","grey")) +
    labs(shape = "model prediction") +
    scale_colour_gradient2(low="blue" ,mid="green", high="red", midpoint=0.5, breaks=c(0,1), labels=c("Low","High")) +
    geom_text(aes(x = feature, y=-Inf, label = sprintf("%.3f", mean_phi)), hjust = -0.2, alpha = 0.7, color = "black") +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position="right") +
    geom_hline(yintercept = 0, color = "grey") + # the vertical line
    labs(y = "SHAP decision plot - test set", x = "features", color = "feature values scaled\n to [low=0 high=1]") +
    theme(text = element_text(size = 10, family="Helvetica"),
          # Remove panel border
          panel.border = element_blank(),
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"),
          legend.key.width = grid::unit(2,"mm")) +
    ylim(min(shap_Mean$Phi)-0.05, max(shap_Mean$Phi)+0.05)

  shap_plot <- ggplotly(shap_plot)

  return(list(shap_plot, shap_Mean_wide, shap_Mean, shap))
}
