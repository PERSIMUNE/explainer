# Enhanced SHAP Analysis for Binary Classification Models

The SHAP plot for classification models is a visualization tool that
uses the Shapley value, an approach from cooperative game theory, to
compute feature contributions for single predictions. The Shapley value
fairly distributes the difference of the instance’s prediction and the
datasets average prediction among the features. This method is available
from the iml package.

## Usage

``` r
eSHAP_plot(
  task,
  trained_model,
  splits,
  sample.size = 30,
  seed = 246,
  subset = 1
)
```

## Arguments

- task:

  mlr3 task object for binary classification

- trained_model:

  mlr3 trained learner object

- splits:

  mlr3 object defining data splits for train and test sets

- sample.size:

  numeric, default to 30. The larger the value, the slower but more
  accurate the estimate of SHAP values

- seed:

  numeric, an integer for reproducibility. Default to 246

- subset:

  numeric, what percentage of the instances to use from 0 to 1 where 1
  means all

## Value

A list containing:

- shap_plot:

  An enhanced SHAP plot with user interactive elements.

- shap_Mean_wide:

  A matrix of SHAP values.

- shap_Mean:

  A data.table with aggregated SHAP values.

- shap:

  Raw SHAP values.

- shap_pred_plot:

  A plot depicting SHAP values versus predicted probabilities.

## References

Zargari Marandi, R., 2024. ExplaineR: an R package to explain machine
learning models. Bioinformatics advances, 4(1), p.vbae049. Molnar C,
Casalicchio G, Bischl B. iml: An R package for interpretable machine
learning. Journal of Open Source Software. 2018 Jun 27;3(26):786.

## See also

[`eSHAP_plot_reg()`](https://persimune.github.io/explainer/reference/eSHAP_plot_reg.md)

Other classification:
[`eSHAP_plot_multiclass()`](https://persimune.github.io/explainer/reference/eSHAP_plot_multiclass.md)

Other SHAP:
[`eSHAP_plot_multiclass()`](https://persimune.github.io/explainer/reference/eSHAP_plot_multiclass.md)

## Examples

``` r
# \donttest{
library("explainer")
seed <- 246
set.seed(seed)
# Load necessary packages
if (!requireNamespace("mlbench", quietly = TRUE)) stop("mlbench not installed.")
if (!requireNamespace("mlr3learners", quietly = TRUE)) stop("mlr3learners not installed.")
if (!requireNamespace("ranger", quietly = TRUE)) stop("ranger not installed.")
# Load BreastCancer dataset
utils::data("BreastCancer", package = "mlbench")
target_col <- "Class"
positive_class <- "malignant"
mydata <- BreastCancer[, -1]
mydata <- na.omit(mydata)
sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
mydata$age <- as.numeric(sample(seq(18, 60), size = nrow(mydata), replace = TRUE))
mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))
maintask <- mlr3::TaskClassif$new(
  id = "my_classification_task",
  backend = mydata,
  target = target_col,
  positive = positive_class
)
splits <- mlr3::partition(maintask)
mylrn <- mlr3::lrn("classif.ranger", predict_type = "prob")
mylrn$train(maintask, splits$train)
SHAP_output <- eSHAP_plot(
  task = maintask,
  trained_model = mylrn,
  splits = splits,
  sample.size = 2, # also 30 or more
  seed = seed,
  subset = 0.02 # up to 1
)
# }
```
