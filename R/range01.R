#' @title Data scale to 0 and 1
#' @description
#' Scale the data to the range of 0 to 1. It uses the Hampel filter to adjust outliers, followed by min-max normalization.
#'
#' @param x Vector or array of numbers to be normalized
#'
#' @importFrom stats median mad
#' @export
#'
#' @return Normalized vector
#'
#' @examples
#' normalized_vector <- range01(seq(-10:1000))
#'
#' @references
#' Pearson, R. K. (1999). “Data cleaning for dynamic modeling and control”. European Control Conference, ETH Zurich, Switzerland.
#'
#' @seealso
#' [eSHAP_plot()]
#'
#' @keywords internal
#' @family data preprocessing
range01 <- function(x) {
  lower_bound <- median(x, na.rm = TRUE) - 3 * mad(x, na.rm = TRUE, constant = 1)
  upper_bound <- median(x, na.rm = TRUE) + 3 * mad(x, na.rm = TRUE, constant = 1)

  if (upper_bound != lower_bound) {
    x[x > upper_bound] <- upper_bound
    x[x < lower_bound] <- lower_bound
  }

  scaled_x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(scaled_x)
}
