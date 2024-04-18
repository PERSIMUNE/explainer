#' @title SHAP clustering
#' @description SHAP values are used to cluster data samples using the k-means method to identify subgroups of individuals with specific patterns of feature contributions.
#'
#' @param task an mlr3 task for binary classification
#' @param trained_model an mlr3 trained learner object
#' @param splits an mlr3 object defining data splits for train and test sets
#' @param shap_Mean_wide the data frame of SHAP values in wide format from eSHAP_plot.R
#' @param shap_Mean_long the data frame of SHAP values in long format from eSHAP_plot.R
#' @param num_of_clusters number of clusters to make based on SHAP values, default: 4
#' @param seed an integer for reproducibility, Default to 246
#' @param subset what percentage of the instances to use from 0 to 1 where 1 means all
#' @param algorithm k-means algorithm character: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen".
#' @param iter.max maximum number of iterations allowed
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggtitle ggplot aes geom_violin geom_line coord_flip geom_jitter position_jitter scale_shape_manual labs scale_colour_gradient2 geom_text theme geom_hline element_blank element_text element_line ylim facet_wrap ggsave
#' @importFrom plotly ggplotly
#' @importFrom tibble tibble as_tibble
#' @importFrom writexl write_xlsx
#' @importFrom data.table data.table melt := .SD
#' @importFrom cvms confusion_matrix plot_confusion_matrix
#' @importFrom ggpubr ggarrange
#' @importFrom stats kmeans
#' @importFrom utils capture.output
#' @export
#'
#' @return A list containing four elements:
#' \item{shap_plot_onerow}{An interactive plot displaying the SHAP values for each feature, clustered by the specified number of clusters. Each cluster is shown in a facet.}
#' \item{combined_plot}{A ggplot2 figure combining confusion matrices for each cluster, providing insights into the model's performance within each identified subgroup.}
#' \item{kmeans_fvals_desc}{A summary table containing statistical descriptions of the clusters based on feature values.}
#' \item{shap_Mean_wide_kmeans}{A data frame containing clustered SHAP values along with predictions and ground truth information.}
#' \item{kmeans_info}{Information about the k-means clustering process, including cluster centers and assignment details.}
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
#' SHAP_output <- eSHAP_plot(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   sample.size = 2, # also 30 or more
#'   seed = seed,
#'   subset = 0.02 # up to 1
#' )
#' shap_Mean_wide <- SHAP_output[[2]]
#' shap_Mean_long <- SHAP_output[[3]]
#' SHAP_plot_clusters <- SHAPclust(
#'   task = maintask,
#'   trained_model = mylrn,
#'   splits = splits,
#'   shap_Mean_wide = shap_Mean_wide,
#'   shap_Mean_long = shap_Mean_long,
#'   num_of_clusters = 3, # your choice
#'   seed = seed,
#'   subset = 0.02, # match with eSHAP_plot
#'   algorithm="Hartigan-Wong",
#'   iter.max = 10
#' )
#' }
#'
#' @references
#' Zargari Marandi, R., 2024. ExplaineR: an R package to explain machine learning models. Bioinformatics advances, 4(1), p.vbae049, https://doi.org/10.1093/bioadv/vbae049.
#'
#' @seealso Other functions to visualize and interpret machine learning models: \code{\link{eSHAP_plot}}.
#'
#' @keywords clustering SHAP k-means machine-learning interpretability
SHAPclust <- function(task,
                      trained_model,
                      splits,
                      shap_Mean_wide,
                      shap_Mean_long,
                      num_of_clusters = 4,
                      seed = 246,
                      subset = 1,
                      algorithm="Hartigan-Wong",
                      iter.max = 1000
                      ){
  cluster <- NULL
  correct_prediction <- NULL
  feature <- NULL
  f_val <- NULL
  fval <- NULL
  mean_absolute_shap <- NULL
  mean_phi <- NULL
  Phi <- NULL
  pred_class <- NULL
  pred_prob <- NULL
  prediction_correctness <- NULL
  response <- NULL
  sample_num <- NULL
  truth <- NULL
  unscaled_f_val <- NULL
  value <- NULL
  variable <- NULL

  mydata <- task$data()
  # randomly subset the target variable and the corresponding rows

  set.seed(seed) # set seed for reproducibility
  n <- round(subset * length(splits$test))
  target_index <- sample(splits$test, size = n, replace = FALSE)
  mydata <- mydata[target_index, ]

  # do the prediction for the test set
  pred_results <- trained_model$predict(task,target_index)

  # the test set based on the data split is used to calculate SHAP values
  test_set <- as.data.frame(mydata)

  km.res <- kmeans(shap_Mean_wide[,!c("sample_num")], centers =  num_of_clusters, nstart = 20, algorithm=algorithm, iter.max = iter.max)
  kmeans_info <- km.res
  # capture.output(km.res, file =  paste0('kmeans_info',seed,'.txt'))

  shap_Mean_wide_kmeans <-
    shap_Mean_wide %>%
    mutate(cluster = km.res$cluster)

  shap_Mean_wide_kmeans <- cbind(data.table::as.data.table(pred_results),shap_Mean_wide_kmeans)
  kmeans_fvals <- cbind(shap_Mean_wide_kmeans$cluster,test_set)
  colnames(kmeans_fvals)[1] <- "cluster"

  # save the statistical descriptions of the clusters by feature values
  kmeans_fvals_desc <- psych::describeBy(kmeans_fvals,group=kmeans_fvals$cluster)
  shap_Mean_wide_kmeans$row_ids <- shap_Mean_wide_kmeans$row_ids - shap_Mean_wide_kmeans$row_ids[1] + 1
  shap_Mean_wide_kmeans[, prediction_correctness := (truth == response)]
  shap_Mean_wide_kmeans_forCM <- shap_Mean_wide_kmeans

  shap_Mean_wide_kmeans[,c(1,2,5)] <- NULL # ,3,4
  colnames(shap_Mean_wide_kmeans)[2] <- "prob_positive_class"
  variables_for_long_format <- colnames(shap_Mean_wide_kmeans)

  variables_for_long_format <- variables_for_long_format[!variables_for_long_format %in% c("sample_num", "prediction_correctness", "cluster","response","prob_positive_class")]

  # Melt the data.table from wide to long format
  dt_long <- data.table::melt(shap_Mean_wide_kmeans,
                              id.vars = c("sample_num", "prediction_correctness", "cluster","response","prob_positive_class"),
                              measure.vars = variables_for_long_format,
                              variable.name = "variable",
                              value.name = "value")

  # Remove specified columns
  dt_long[, c("response", "prob_positive_class", "prediction_correctness") := NULL]
  # Rename columns
  names(dt_long)[names(dt_long) == "variable"] <- "feature"
  names(dt_long)[names(dt_long) == "value"] <- "Phi"

  # Merge the two dataframes
  dt_long <- merge(dt_long, shap_Mean_long, by = c("sample_num", "feature", "Phi"))
  print(dt_long)
  ############## SHAP plots for clusters
  shap_plot1 <- dt_long %>%
    mutate(feature = forcats::fct_reorder(feature, mean_phi)) %>%
    ggplot(aes(x = feature, y = Phi, color = f_val))+
    geom_violin(colour = "grey") +
    geom_line(aes(group = sample_num), alpha = 0.1,size=0.2) +
    coord_flip() +
    geom_jitter(aes(shape=correct_prediction, text = paste("Feature: ", feature,
                                                           "<br>Unscaled feature value: ", unscaled_f_val,
                                                           "<br>SHAP value: ", Phi,
                                                           "<br>Prediction correctness: ", correct_prediction,
                                                           "<br>Predicted probability: ", pred_prob,
                                                           "<br>Predicted class: ", pred_class)),
                alpha = 0.6, size=1.5, position=position_jitter(width=0.2, height=0)) +
    scale_shape_manual(values = c(4, 19)) +  # 19 for correct predictions (circle), 4 for incorrect predictions (cross)
    labs(shape = "model prediction") +
    scale_colour_gradient2(low = "blue", mid = "green", high = "red", midpoint = 0.5, breaks = c(0, 1), labels = c("Low", "High")) +
    geom_text(aes(x = feature, y = -Inf, label = ""), hjust = -0.2, alpha = 0.7, color = "black") +
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "right") +
    geom_hline(yintercept = 0, color = "grey", alpha = 0.5) +
    labs(y = "SHAP decision plot - test set", x = "features", color = "feature values scaled\n to [low=0 high=1]") +
    theme(text = element_text(size = 8, family = "Helvetica"), panel.border = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.line = element_line(colour = "grey"), legend.key.width = grid::unit(2, "mm")) +
    ylim(min(dt_long$Phi) - 0.05, max(dt_long$Phi) + 0.05) +
    guides(
      shape = ggplot2::guide_legend(color = "black")
    )

  shap_plot_onerow <- shap_plot1 + facet_wrap(~ cluster, ncol = num_of_clusters)

  shap_plot_onerow <- ggplotly(shap_plot_onerow, tooltip="text")

  CM_plt <- list()
  # Create a tibble for each cluster and calculate the confusion matrix for each cluster
  for (i in 1:num_of_clusters) {
    d_binomial <- tibble::tibble("Truth" = shap_Mean_wide_kmeans_forCM$truth[which(shap_Mean_wide_kmeans_forCM$cluster==i)],
                         "Prediction" = shap_Mean_wide_kmeans_forCM$response[which(shap_Mean_wide_kmeans_forCM$cluster==i)])
    cvms::confusion_matrix(targets = d_binomial$Truth, predictions = d_binomial$Prediction)
    # basic_table <- table(d_binomial)
    # cfm <- cvms::tidy(basic_table)
    cfm <- cvms::confusion_matrix(targets = d_binomial$Truth, predictions = d_binomial$Prediction)

    cm_tbl <- data.frame(matrix(nrow = 4, ncol = 3))
    colnames(cm_tbl) <- c("Target", "Prediction", "N")
    cm_tbl <- tibble::as_tibble(cm_tbl)
    cm_tbl[1:2,1] <- levels(d_binomial$Truth)[1]
    cm_tbl[3:4,1] <- levels(d_binomial$Truth)[2]
    cm_tbl[1,2] <- levels(d_binomial$Truth)[1]
    cm_tbl[1,3] <- sum(d_binomial$Truth %in% cm_tbl[1,1] & d_binomial$Prediction %in% cm_tbl[1,2])
    cm_tbl[2,2] <- levels(d_binomial$Truth)[2]
    cm_tbl[2,3] <- sum(d_binomial$Truth %in% cm_tbl[2,1] & d_binomial$Prediction %in% cm_tbl[2,2])
    cm_tbl[3,2] <- levels(d_binomial$Truth)[1]
    cm_tbl[3,3] <- sum(d_binomial$Truth %in% cm_tbl[3,1] & d_binomial$Prediction %in% cm_tbl[3,2])
    cm_tbl[4,2] <- levels(d_binomial$Truth)[2]
    cm_tbl[4,3] <- sum(d_binomial$Truth %in% cm_tbl[4,1] & d_binomial$Prediction %in% cm_tbl[4,2])

    # Plot the confusion matrix for each cluster
    CM_plt[[i]] <- cvms::plot_confusion_matrix(conf_matrix = cm_tbl) # cfm$`Confusion Matrix`[[1]] doesn't work when only one level is the truth
    CM_plt[[i]][["theme"]][["text"]][["size"]] <- 6
    CM_plt[[i]][["theme"]][["axis.text"]][["size"]] <- 6
    CM_plt[[i]][["theme"]][["text"]][["family"]] <- 'Helvetica'

    # Add the cluster number to the title of each plot
    CM_plt[[i]] <- CM_plt[[i]] + ggtitle(paste("Cluster", i)) + theme(plot.title = element_text(hjust = 0.5))
  }

  # Combine the plots into one figure
  combined_plot <- ggpubr::ggarrange(plotlist = CM_plt, ncol = num_of_clusters)

  return(list(shap_plot_onerow, combined_plot, kmeans_fvals_desc, shap_Mean_wide_kmeans, kmeans_info))

}
