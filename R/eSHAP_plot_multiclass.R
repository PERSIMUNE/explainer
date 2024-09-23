#' @title Enhanced SHAP Analysis for Multi-Class Classification Models
#'
#' @description
#' The SHAP plot for multi-class classification models is a visualization tool that uses the Shapley value to compute feature contributions for single predictions across multiple classes.
#'
#' @param sample.size numeric, default to 30. The larger the value, the slower but more accurate the estimate of SHAP values
#' @param seed numeric, an integer for reproducibility. Default to 246
#' @param task mlr3 task object for multi-class classification
#' @param trained_model mlr3 trained learner object
#' @param splits mlr3 object defining data splits for train and test sets
#' @param subset numeric, what percentage of the instances to use from 0 to 1 where 1 means all
#'
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot coord_flip scale_color_gradient labs theme_minimal theme ylim
#' @importFrom stats aggregate
#' @importFrom scales rescale
#' @importFrom gridExtra grid.arrange
#' @export
#'
#' @return
#' A list containing:
#' \item{combined_plots}{SHAP plot depicting the SHAP values for each class}
#' \item{shap_data}{A matrix of SHAP values for each class.}
#' \item{combined_all_classes}{overall SHAP plot depicting the SHAP values for all classes on a single plot}
#'
#' @examples
#' \donttest{
#' library("explainer")
#' seed <- 246
#' set.seed(seed)
#' # Load necessary packages and data...
#' }
#'
#' @keywords internal
#' @family classification
#' @family SHAP
eSHAP_plot_multiclass <- function(task,
                                  trained_model,
                                  splits,
                                  sample.size = 30,
                                  seed = 246,
                                  subset = 1) {
  feature <- NULL
  phi <- NULL
  scaled_value <- NULL

  set.seed(seed)  # Set seed for reproducibility
  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  # Separate features and target variable
  X <- mydata[splits$train, which(names(mydata) != task$target_names)]
  y <- mydata[splits$train, task$target_names]

  # Define model for SHAP calculation
  model <- iml::Predictor$new(trained_model, data = X, y = y)

  # Sample subset of test data for SHAP calculations
  n <- round(subset * length(splits$test))
  target_index <- splits$test[sample(seq_along(splits$test), size = n, replace = FALSE)]
  test_set <- mydata[target_index, ]

  # Initialize list to store SHAP values
  shap_values <- list()

  # Calculate SHAP values for each class and store them
  for (class in levels(test_set[[task$target_names]])) {
    class_data <- test_set[test_set[[task$target_names]] == class, ]
    if (nrow(class_data) > 0) {
      class_model <- iml::Shapley$new(model,
                                      x.interest = class_data[, names(X)],
                                      sample.size = sample.size)
      shap_values[[class]] <- class_model$results
    }
  }

  # Combine SHAP results into a single data frame, and add class labels
  shap_data <- do.call(rbind, lapply(names(shap_values), function(class) {
    df <- shap_values[[class]]
    df$class <- class
    return(df)
  }))

  # Calculate mean absolute SHAP values for feature ordering
  shap_data$abs_phi <- abs(shap_data$phi)

  # Calculate the scaled feature values globally (across all classes)
  shap_data$scaled_value <- rescale(as.numeric(as.factor(shap_data$feature.value)), to = c(0, 1))

  # Create a list to store plots for each class
  shap_plots <- list()

  for (class in levels(test_set[[task$target_names]])) {
    class_data <- shap_data[shap_data$class == class, ]

    # Calculate mean absolute SHAP values for feature ranking
    mean_abs_shap <- aggregate(abs_phi ~ feature, data = class_data, FUN = mean)
    mean_abs_shap <- mean_abs_shap[order(mean_abs_shap$abs_phi, decreasing = TRUE), ]

    # Reorder features based on mean absolute SHAP values
    class_data$feature <- factor(class_data$feature, levels = mean_abs_shap$feature)

    # Create the plot for the current class
    plot <- ggplot(class_data, aes(x = feature, y = phi)) +
      geom_point(aes(color = scaled_value), alpha = 0.6) +  # Display all instances with color mapping
      geom_boxplot(outlier.shape = NA, fill = "lightgray", alpha = 0.6) +  # Boxplot for distribution
      coord_flip() +  # Flip coordinates for better readability
      labs(y = paste("SHAP value (Class:", class, ")"), x = "features", color = "feature value") +
      scale_color_gradient(low = "blue", high = "red") +  # Color gradient from blue to red
      theme_minimal() +
      ylim(min(shap_data$phi), max(shap_data$phi))  # Set y-axis limits

    shap_plots[[class]] <- plot
  }

  # Create a combined plot with all classes, using different shapes for each class
  combined_all_classes <- ggplot(shap_data, aes(x = feature, y = phi, shape = class)) +
    geom_point(aes(color = scaled_value), alpha = 0.6, size = 2) +  # Display all instances with color mapping
    geom_boxplot(outlier.shape = NA, fill = "lightgray", alpha = 0.6) +  # Boxplot for distribution
    coord_flip() +  # Flip coordinates for better readability
    labs(y = "SHAP value", x = "features", color = "feature value", shape = "Class") +
    scale_color_gradient(low = "blue", high = "red") +  # Color gradient from blue to red
    theme_minimal() +
    theme(legend.position = "right")  # Ensure the legend is on the right

  # Combine the individual SHAP plots into a grid layout
  combined_plots <- grid.arrange(grobs = shap_plots, ncol = 1)

  return(list(combined_plot = combined_plots, shap_data = shap_data, combined_all_classes = combined_all_classes))
}
