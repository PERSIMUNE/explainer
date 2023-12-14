#' @title Decision Curve Plot
#'
#' @description
#' Decision curve analysis is a statistical method used in medical research to evaluate and compare the clinical utility of different diagnostic or predictive models. It assesses the net benefit of a model across a range of decision thresholds, aiding in the selection of the most informative and practical approach for guiding clinical decisions.
#'
#' @param task mlr3 task object specifying the task details
#' @param trained_model mlr3 trained learner (model) object obtained after training
#' @param splits mlr3 object defining data splits for train and test sets
#' @param seed numeric, seed for reproducibility (default: 246)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual geom_vline ylim labs theme element_blank
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly
#' @export
#'
#' @return An interactive decision curve plot
#'
#' @examples
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
#' sex <- sample(
#'   c("Male", "Female"),
#'   size = nrow(mydata),
#'   replace = TRUE
#' )
#' mydata$age <- as.numeric(sample(
#'   seq(18,60),
#'   size = nrow(mydata),
#'   replace = TRUE
#' ))
#' mydata$sex <- factor(
#'   sex,
#'   levels = c("Male", "Female"),
#'   labels = c(1, 0)
#' )
#' maintask <- mlr3::TaskClassif$new(
#'   id = "my_classification_task",
#'   backend = mydata,
#'   target = target_col,
#'   positive = positive_class
#' )
#' splits <- mlr3::partition(maintask)
#' mylrn <- mlr3::lrn(
#'   "classif.ranger",
#'   predict_type = "prob"
#' )
#' mylrn$train(maintask, splits$train)
#' myplot <- eDecisionCurve(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   seed = seed
#' )
eDecisionCurve <- function(task,
                           trained_model,
                           splits,
                           seed = 246) {
  threshold <- NULL
  value <- NULL
  variable <- NULL

  mydata <- task$data()
  mydata <- as.data.frame(mydata)

  featset_total_test <- mydata[splits$test,]
  featset_total_test <- as.data.frame(featset_total_test)

  pred_results <- data.table::as.data.table(trained_model$predict(task, splits$test))
  pred_results <- as.data.frame(pred_results)

  positive_class_label <- task$positive
  negative_class_label <- task$negative
  outcome <- task$target_names
  poscol <- paste0("prob.", task$positive)

  x <- 1
  PLodds <- c()
  fneg <- c()
  fpos <- c()
  Tneg <- c()
  Tpos <- c()
  NBsensitivity <- c()
  NBspecificity <- c()
  netbenefit <- c()

  Tpos_ta <- c()
  fpos_ta <- c()
  Tpos_no <- c()
  fpos_no <- c()
  Tpos_rand <- c()
  fpos_rand <- c()
  total_all <- c()
  total_none <- c()
  total_rand <- c()

  for (i in seq(0.01, 0.99, 0.01)) {
    fpos[x] <- sum(featset_total_test[, outcome] == negative_class_label & pred_results[, poscol] >= i, na.rm = TRUE)
    Tpos[x] <- sum(featset_total_test[, outcome] == positive_class_label & pred_results[, poscol] >= i, na.rm = TRUE)

    Tpos_ta[x] <- sum(featset_total_test[, outcome] == positive_class_label)
    fpos_ta[x] <- sum(featset_total_test[, outcome] == negative_class_label)

    Tpos_no[x] <- 0
    fpos_no[x] <- 0

    set.seed(seed + x)
    randpred <- sample(c(positive_class_label, negative_class_label), nrow(featset_total_test), replace = TRUE)
    Tpos_rand[x] <- sum(featset_total_test[, outcome] == positive_class_label & randpred == positive_class_label)
    fpos_rand[x] <- sum(featset_total_test[, outcome] == negative_class_label & randpred == positive_class_label)

    netbenefit[x] <- Tpos[x] / nrow(featset_total_test) - ((fpos[x] * (i / (1 - i))) / nrow(featset_total_test))
    total_all[x] <- Tpos_ta[x] / nrow(featset_total_test) - ((fpos_ta[x] * (i / (1 - i))) / nrow(featset_total_test))
    total_none[x] <- Tpos_no[x] / nrow(featset_total_test) - ((fpos_no[x] * (i / (1 - i))) / nrow(featset_total_test))
    total_rand[x] <- Tpos_rand[x] / nrow(featset_total_test) - ((fpos_rand[x] * (i / (1 - i))) / nrow(featset_total_test))
    x <- x + 1
  }

  netbenefit_df <- data.frame(netbenefit = netbenefit,
                              total_all = total_all,
                              total_none = total_none,
                              total_rand = total_rand,
                              threshold = seq(0.01, 0.99, 0.01))
  netbenefit_df_long <- reshape2::melt(netbenefit_df, id.vars = c("threshold"))

  decision_curve_plt <- ggplot(data = netbenefit_df_long, aes(x = threshold, y = value, colour = variable)) +
    geom_point(alpha = 0.5) +
    geom_line(linetype = "longdash", alpha = 0.5) +
    scale_color_manual(values = c("red", "grey", "black", "green"), labels = c("proposed model", paste0("classify all as ", positive_class_label), paste0("classify all as ", negative_class_label), "random guess")) +
    geom_vline(xintercept = 0.5, colour = "blue", linetype = "longdash") +
    ylim(0, sum(featset_total_test[, outcome] == positive_class_label) / nrow(featset_total_test)) +
    labs(title = "Decision curve analysis", subtitle = paste0("number of patients in the test set N=", nrow(featset_total_test))) +
    egg::theme_article(base_size = 10) +
    theme(legend.title = element_blank())

  decision_curve_plt$labels$x[1] <- "probability threshold"
  decision_curve_plt$labels$y[1] <- "net benefit"

  decision_curve_plt <- ggplotly(decision_curve_plt)
  return(decision_curve_plt)
}
