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
#' data("BreastCancer", package = "mlbench")
#' mydata <- BreastCancer[, -1]
#' mydata <- na.omit(mydata)
#' sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
#' mydata$age <- sample(seq(18, 60), size = nrow(mydata), replace = TRUE)
#' mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
#' mydata$Class <- NULL
#' mydata$Cl.thickness <- as.numeric(mydata$Cl.thickness)
#' target_col <- "Cl.thickness"
#' maintask <- mlr3::TaskRegr$new(id = "my_regression_task",backend = mydata,target = target_col)
#' set.seed(seed)
#' splits <- mlr3::partition(maintask)
#' library("mlr3extralearners")
#' mylrn <- mlr3::lrn("regr.randomForest", predict_type = "response")
#' mylrn$train(maintask, splits$train)
#' reg_model_outputs <- mylrn$predict(maintask, splits$test)
#' SHAP_output <- eSHAP_plot_reg(task = maintask, trained_model = mylrn, splits = splits, sample.size = 30, seed = seed, subset = 0.8)
#' myplot <- SHAP_output[[1]]
#' }
eSHAP_plot_reg <- function(task,
                           trained_model,
                           splits,
                           sample.size = 30,
                           seed = 246,
                           subset = 1) {
  mydata <- task$data()
  mydata <- as.data.frame(mydata)
  X <- mydata[which(names(mydata[splits$train, ]) != task$target_names)]
  model <- iml::Predictor$new(trained_model, data = X, y = mydata[, task$target_names])

  if (subset < 1) {
    set.seed(seed)
    n <- round(subset * length(splits$test))
    target_index <- sample(splits$test, size = n, replace = FALSE)
    mydata <- mydata[target_index, ]
  } else {
    mydata <- mydata[splits$test, ]
  }

  feature_names <- colnames(X)
  shap_values <- vector("list", nrow(mydata))

  for (i in seq_along(shap_values)) {
    set.seed(seed)
    shap_values[[i]] <- iml::Shapley$new(model, x.interest = mydata[i, feature_names], sample.size = sample.size)$results
  }

  data_shap_values <- dplyr::bind_rows(shap_values)
  shap_Mean <- data.table::data.table(feature = rep(feature_names, each = sample.size),
                                      Phi = abs(data_shap_values$phi),
                                      f_val = data_shap_values$feature.value,
                                      sample_num = rep(1:nrow(mydata), length(feature_names)))

  shap_plot <- shap_Mean %>%
    ggplot(aes(x = feature, y = Phi, color = f_val)) +
    geom_violin(colour = "grey") +
    geom_line(aes(group = sample_num), alpha = 0.1, size = 0.2) +
    coord_flip() +
    geom_jitter(alpha = 0.6, size = 1, position = position_jitter(width = 0.2, height = 0)) +
    scale_colour_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0.5, breaks = c(0, 1), labels = c("Low", "High")) +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "right") +
    geom_hline(yintercept = 0, color = "grey") +
    labs(y = "SHAP decision plot - test set", x = "features", color = "feature values scaled to [low=0 high=1]") +
    theme(text = element_text(size = 10, family = "Helvetica"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "grey"),
          legend.key.width = grid::unit(2, "mm"))

  shap_plot <- ggplotly(shap_plot)

  return(list(shap_plot, shap_Mean))
}
