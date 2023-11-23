#' @title SHAP values versus feature values
#' @description SHAP values in association with feature values
#'
#' @param shap_Mean_long the data frame containing SHAP values in long format
#'
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab facet_wrap
#' @importFrom ggplot2 stat_smooth
#' @importFrom egg theme_article
#' @importFrom plotly ggplotly
#'
#' @return interactive plot of SHAP values in association with feature values
#' @export
#'
#' @examples
#' \dontrun{
#' # To see detailed examples, refer to the example tutorials in the package vignettes.
#' }
ShapFeaturePlot <- function(shap_Mean_long){
  # utils::globalVariables(c("f_val", "Phi", "correct_prediction"))
  f_val <- NULL
  Phi <- NULL
  correct_prediction <- NULL
  SHAP_vs_FVAL_plt <- ggplot(shap_Mean_long, aes(x = f_val, y = Phi)) +
    geom_point(aes(shape = correct_prediction), size = 1, alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ x) +
    xlab("normalized feature values [0 1]") +
    ylab("SHAP values") +
    facet_wrap(~ feature, scales = "free_y") +
    egg::theme_article() # base_size = 8

  SHAP_vs_FVAL_plt <- ggplotly(SHAP_vs_FVAL_plt)
  # display the plot
  return(SHAP_vs_FVAL_plt)
}
