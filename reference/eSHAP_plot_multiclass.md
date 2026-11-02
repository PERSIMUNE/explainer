# Enhanced SHAP Analysis for Multi-Class Classification Models

The SHAP plot for multi-class classification models is a visualization
tool that uses the Shapley value to compute feature contributions for
single predictions across multiple classes.

## Usage

``` r
eSHAP_plot_multiclass(
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

  mlr3 task object for multi-class classification

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

- combined_plots:

  SHAP plot depicting the SHAP values for each class

- shap_data:

  A matrix of SHAP values for each class.

- combined_all_classes:

  overall SHAP plot depicting the SHAP values for all classes on a
  single plot

## See also

Other classification:
[`eSHAP_plot()`](https://persimune.github.io/explainer/reference/eSHAP_plot.md)

Other SHAP:
[`eSHAP_plot()`](https://persimune.github.io/explainer/reference/eSHAP_plot.md)

## Examples

``` r
# \donttest{
library("explainer")
seed <- 246
set.seed(seed)
# Load necessary packages and data...
# }
```
