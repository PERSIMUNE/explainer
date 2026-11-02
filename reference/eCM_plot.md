# Enhanced Confusion Matrix Plot

This function generates an enhanced confusion matrix plot using the CVMS
package. The plot includes visualizations of sensitivity, specificity,
positive predictive value (PPV), and negative predictive value (NPV).

## Usage

``` r
eCM_plot(task, trained_model, splits, add_sums = TRUE, palette = "Greens")
```

## Arguments

- task:

  mlr3 task object specifying the task details

- trained_model:

  mlr3 trained learner (model) object obtained after training

- splits:

  mlr3 object defining data splits for train and test sets

- add_sums:

  logical, indicating whether total numbers should be displayed in the
  plot (default: TRUE)

- palette:

  character, the color palette for the confusion matrix (default:
  "Greens")

## Value

A confusion matrix plot visualizing sensitivity, specificity, PPV, and
NPV

## Examples

``` r
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
sex <- sample(
  c("Male", "Female"),
  size = nrow(mydata),
  replace = TRUE
)
mydata$age <- as.numeric(sample(
  seq(18, 60),
  size = nrow(mydata),
  replace = TRUE
))
mydata$sex <- factor(
  sex,
  levels = c("Male", "Female"),
  labels = c(1, 0)
)
maintask <- mlr3::TaskClassif$new(
  id = "my_classification_task",
  backend = mydata,
  target = target_col,
  positive = positive_class
)
splits <- mlr3::partition(maintask)
mylrn <- mlr3::lrn(
  "classif.ranger",
  predict_type = "prob"
)
mylrn$train(maintask, splits$train)
myplot <- eCM_plot(
  task = maintask,
  trained_model = mylrn,
  splits = splits
)
#> Warning: 'ggimage' is missing. Will not plot arrows and zero-shading.
#> Warning: 'rsvg' is missing. Will not plot arrows and zero-shading.
#> Warning: 'ggnewscale' is missing. Will not use palette for sum tiles.
#> Warning: 'ggimage' is missing. Will not plot arrows and zero-shading.
#> Warning: 'rsvg' is missing. Will not plot arrows and zero-shading.
#> Warning: 'ggnewscale' is missing. Will not use palette for sum tiles.
```
