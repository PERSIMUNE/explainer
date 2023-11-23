#' @title Data Scale to 0 and 1
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
#' \dontrun{
#' range01(seq(-10:1000))
#' }
range01 <- function(x) {
  lower_bound <- median(x, na.rm = TRUE) - 3 * mad(x, na.rm = TRUE, constant = 1)
  upper_bound <- median(x, na.rm = TRUE) + 3 * mad(x, na.rm = TRUE, constant = 1)

  if (upper_bound != lower_bound) {
    x[which(x > upper_bound)] <- upper_bound
    x[which(x < lower_bound)] <- lower_bound
  }

  scaled_x <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(scaled_x)
}
