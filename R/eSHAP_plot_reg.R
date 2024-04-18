#' @title Enhanced SHAP Analysis for Regression Models
#'
#' @description
#' The SHAP plot for regression models is a visualization tool that uses the Shapley value, an approach from cooperative game theory, to compute feature contributions for single predictions. The Shapley value fairly distributes the difference of the instance’s prediction and the datasets average prediction among the features. This method is available from the iml package.
#'
#' @param task mlr3 regression task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#' @param sample.size numeric, number of samples to calculate SHAP values (default: 30)
#' @param seed numeric, seed for reproducibility (default: 246)
#' @param subset numeric, proportion of the test set to use for visualization (default: 1)
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_violin geom_line coord_flip geom_jitter position_jitter scale_colour_gradient2 geom_text theme element_blank geom_hline labs element_text element_line ylim
#' @export
#'
#' @return A list of two objects:
#' 1) An enhanced SHAP plot with user interactive elements,
#' 2) A matrix of SHAP values
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
#' mydata <- BreastCancer[, -1]
#' mydata <- na.omit(mydata)
#' sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
#' mydata$age <- sample(seq(18, 60), size = nrow(mydata), replace = TRUE)
#' mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
#' mydata$Class <- NULL
#' mydata$Cl.thickness <- as.numeric(mydata$Cl.thickness)
#' target_col <- "Cl.thickness"
#' maintask <- mlr3::TaskRegr$new(
#'   id = "my_regression_task",
#'   backend = mydata,
#'   target = target_col
#' )
#' splits <- mlr3::partition(maintask)
#' mylrn <- mlr3::lrn("regr.ranger", predict_type = "response")
#' mylrn$train(maintask, splits$train)
#' reg_model_outputs <- mylrn$predict(maintask, splits$test)
#' SHAP_output <- eSHAP_plot_reg(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   sample.size = 2, # also 30 or more
#'   seed = seed,
#'   subset = 0.02 # up to 1
#' )
#' myplot <- SHAP_output[[1]]
#' }
eSHAP_plot_reg <- function(task,
                           trained_model,
                           splits,
                           sample.size = 30,
                           seed = 246,
                           subset = 1) {
  # utils::globalVariables(c("feature", "sample_num"))
  feature <- NULL
  sample_num <- NULL
  pred_value <- NULL
  mydata <- task$data()
  mydata <- as.data.frame(mydata)
  X <- mydata[which(names(mydata[splits$train,]) != task$target_names)]
  model <- iml::Predictor$new(trained_model, data = X, y = mydata[, task$target_names])

  # randomly subset the target variable and the corresponding rows
  set.seed(seed) # set seed for reproducibility
  n <- round(subset * length(splits$test))
  target_index <- sample(splits$test, size = n, replace = FALSE)
  mydata <- mydata[target_index, ]

  # do the prediction for the test set
  pred_results <- trained_model$predict(task, target_index)

  # the test set based on the data split is used to calculate SHAP values
  test_set <- as.data.frame(mydata)
  feature_names <- colnames(X)
  nfeats <- length(feature_names)

  # save the predicted values
  pred_values <- pred_results$response

  test_set.nolab <- mydata
  # initialize the results list.
  shap_values <- vector("list", nrow(test_set))
  for (i in seq_along(shap_values)) {
    set.seed(seed)
    shap_values[[i]] <- iml::Shapley$new(model, x.interest = test_set[i, feature_names],
                                         sample.size = sample.size)$results
    shap_values[[i]]$sample_num <- i  # identifier to track our instances.
    shap_values[[i]]$pred_value <- pred_values[i]
  }
  data_shap_values <- dplyr::bind_rows(shap_values)  # collapse the list.

  shap <- data_shap_values

  total_reps <- nrow(shap) / nfeats
  mean_phi <- rep(0, nfeats)
  indiv_phi <- rep(0, nfeats)
  f_val_lst <- rep(0, nfeats)
  pred_values_rep <- rep(0, nfeats)

  feature_values <- gsub(".*=", '', shap$feature.value)
  shap$feature.value <- as.numeric(feature_values)
  for (i in 1:nfeats) {
    mean_phi[i] = mean(abs(shap$phi[seq(i, nrow(shap), nfeats)]))
    indiv_phi[i] = list(shap$phi[seq(i, nrow(shap), nfeats)])
    f_val_lst[i] = list(feature_values[seq(i, nrow(shap), nfeats)])
    pred_values_rep[i] = list(shap$pred_value[seq(i, nrow(shap), nfeats)])
  }

  # Convert non-numeric columns in test_set.nolab to numeric
  for (col in colnames(test_set.nolab)) {
    test_set.nolab[[col]] <- as.numeric(test_set.nolab[[col]])
  }

  # store feature values
  unscaled_f_val_lst <- f_val_lst

  # Apply transformation for visualization
  for (i in 1:length(f_val_lst)) {
    unscaled_f_val_lst[[i]] <- mydata[,i] # not scaled
    f_val_lst[[i]] <- range01(test_set.nolab[, i])
  }

  (unscaled_f_val = as.numeric(unlist(unscaled_f_val_lst)))
  (f_val = as.numeric(unlist(f_val_lst)))
  (Phi = unlist(indiv_phi))

  shap_Mean <- data.table::data.table(feature = rep(feature_names, each = total_reps),
                                      mean_phi = rep(mean_phi, each = total_reps),
                                      Phi = Phi,
                                      f_val = f_val,
                                      unscaled_f_val = unscaled_f_val,
                                      sample_num = rep(1:nrow(test_set), length(feature_names)),
                                      pred_value = unlist(pred_values_rep))

  shap_Mean_wide <- data.table::dcast(shap_Mean, sample_num ~ feature, value.var = "Phi")

  # Test set without the target variable
  test_set.nolab[, task$target_names] <- NULL

  # Get the column names of the data frame
  cols <- colnames(test_set.nolab)

  # Loop through each column
  for (col in cols) {
    # Check if the column is numeric
    if (!is.numeric(test_set.nolab[[col]])) {
      # Convert non-numeric columns to numeric
      test_set.nolab[[col]] <- as.numeric(test_set.nolab[[col]])
    }
  }

  # Apply transformation for visualization
  for (i in 1:length(f_val_lst)) {
    f_val_lst[[i]] <- range01(test_set.nolab[, i])
  }

  shap_plot <- shap_Mean %>%
    mutate(feature = forcats::fct_reorder(feature, mean_phi)) %>%
    ggplot(aes(x = feature, y = Phi, color = f_val)) +
    geom_violin(colour = "grey") +
    geom_line(aes(group = sample_num), alpha = 0.1, size = 0.2) +
    coord_flip() +
    geom_jitter(aes(text = paste("Feature: ", feature,
                                 "<br>Unscaled feature value: ", unscaled_f_val,
                                 "<br>SHAP value: ", Phi,
                                 "<br>Predicted outcome value: ", pred_value)),
                alpha = 0.6, size=1.5, position=position_jitter(width=0.2, height=0)) +
    scale_colour_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0.5, breaks = c(0, 1), labels = c("Low", "High")) +
    geom_text(aes(x = feature, y = -Inf, label = sprintf("%.3f", mean_phi)), hjust = -0.2, alpha = 0.7, color = "black") +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "right") +
    geom_hline(yintercept = 0, color = "grey") + # the vertical line
    labs(y = "SHAP decision plot - test set", x = "features", color = "feature values scaled\n to [low=0 high=1]") +
    theme(text = element_text(size = 10, family = "Helvetica"),
          # Remove panel border
          panel.border = element_blank(),
          # Remove panel grid lines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # Remove panel background
          panel.background = element_blank(),
          # Add axis line
          axis.line = element_line(colour = "grey"),
          legend.key.width = grid::unit(2, "mm")) +
    ylim(min(shap_Mean$Phi) - 0.05, max(shap_Mean$Phi) + 0.05)

  # Convert ggplot to Plotly
  shap_plot <- ggplotly(shap_plot, tooltip="text")

  return(list(shap_plot, shap_Mean_wide, shap_Mean, shap))
}
