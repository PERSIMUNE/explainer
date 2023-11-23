#' @title SHAP Partial Plot
#' @description Generates an interactive partial dependence plot based on SHAP values, visualizing the marginal effect of one or two features on the predicted outcome of a machine learning model.
#'
#' @param shap_Mean_long data frame containing SHAP values in long format
#'
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab facet_wrap geom_smooth
#' @importFrom egg theme_article
#' @importFrom plotly ggplotly
#'
#' @return Interactive partial dependence plot
#' @export
#'
#' @examples
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
shapPartialPlot <- function(shap_Mean_long) {
  # utils::globalVariables(c("f_val", "pred_prob", "correct_prediction"))
  f_val <- NULL
  pred_prob <- NULL
  correct_prediction <- NULL
  shapPartialPlot <- ggplot(shap_Mean_long, aes(x = f_val, y = pred_prob)) +
    geom_point(aes(shape = correct_prediction), size = 1, alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x) +
    xlab("Normalized Feature Values [0 1]") +
    ylab("Predicted Probability for the Positive Class") +
    facet_wrap(~ feature, scales = "free_y") +
    egg::theme_article()

  shapPartialPlot <- ggplotly(shapPartialPlot)
  # Display the plot
  return(shapPartialPlot)
}
