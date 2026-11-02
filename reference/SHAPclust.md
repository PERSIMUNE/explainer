# SHAP clustering

SHAP values are used to cluster data samples using the k-means method to
identify subgroups of individuals with specific patterns of feature
contributions.

## Usage

``` r
SHAPclust(
  task,
  trained_model,
  splits,
  shap_Mean_wide,
  shap_Mean_long,
  num_of_clusters = 4,
  seed = 246,
  subset = 1,
  algorithm = "Hartigan-Wong",
  iter.max = 1000
)
```

## Arguments

- task:

  an mlr3 task for binary classification

- trained_model:

  an mlr3 trained learner object

- splits:

  an mlr3 object defining data splits for train and test sets

- shap_Mean_wide:

  the data frame of SHAP values in wide format from eSHAP_plot.R

- shap_Mean_long:

  the data frame of SHAP values in long format from eSHAP_plot.R

- num_of_clusters:

  number of clusters to make based on SHAP values, default: 4

- seed:

  an integer for reproducibility, Default to 246

- subset:

  what percentage of the instances to use from 0 to 1 where 1 means all

- algorithm:

  k-means algorithm character: "Hartigan-Wong", "Lloyd", "Forgy",
  "MacQueen".

- iter.max:

  maximum number of iterations allowed

## Value

A list containing four elements:

- shap_plot_onerow:

  An interactive plot displaying the SHAP values for each feature,
  clustered by the specified number of clusters. Each cluster is shown
  in a facet.

- combined_plot:

  A ggplot2 figure combining confusion matrices for each cluster,
  providing insights into the model's performance within each identified
  subgroup.

- kmeans_fvals_desc:

  A summary table containing statistical descriptions of the clusters
  based on feature values.

- shap_Mean_wide_kmeans:

  A data frame containing clustered SHAP values along with predictions
  and ground truth information.

- kmeans_info:

  Information about the k-means clustering process, including cluster
  centers and assignment details.

## References

Zargari Marandi, R., 2024. ExplaineR: an R package to explain machine
learning models. Bioinformatics advances, 4(1), p.vbae049,
https://doi.org/10.1093/bioadv/vbae049.

## See also

Other functions to visualize and interpret machine learning models:
[`eSHAP_plot`](https://persimune.github.io/explainer/reference/eSHAP_plot.md).

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
SHAP_output <- eSHAP_plot(
  task = maintask,
  trained_model = mylrn,
  splits = splits,
  sample.size = 2, # also 30 or more
  seed = seed,
  subset = 0.02 # up to 1
)
shap_Mean_wide <- SHAP_output[[2]]
shap_Mean_long <- SHAP_output[[3]]
SHAP_plot_clusters <- SHAPclust(
  task = maintask,
  trained_model = mylrn,
  splits = splits,
  shap_Mean_wide = shap_Mean_wide,
  shap_Mean_long = shap_Mean_long,
  num_of_clusters = 3, # your choice
  seed = seed,
  subset = 0.02, # match with eSHAP_plot
  algorithm = "Hartigan-Wong",
  iter.max = 10
)
#> Key: <sample_num, feature, Phi>
#>     sample_num         feature           Phi cluster     mean_phi     f_val
#>          <int>          <char>         <num>   <int>        <num>     <num>
#>  1:          1     Bare.nuclei -0.1399936508       2 0.0493383117 0.0000000
#>  2:          1     Bl.cromatin -0.0828492063       2 0.0489450397 0.0000000
#>  3:          1      Cell.shape -0.0367019841       2 0.0745567280 0.0000000
#>  4:          1       Cell.size  0.0000000000       2 0.0319849206 0.0000000
#>  5:          1    Cl.thickness -0.0178948413       2 0.0262465278 0.5000000
#>  6:          1    Epith.c.size -0.0055142857       2 0.0059076389 0.5000000
#>  7:          1   Marg.adhesion  0.0000000000       2 0.0238763889 0.0000000
#>  8:          1         Mitoses  0.0000000000       2 0.0043198413 0.0000000
#>  9:          1 Normal.nucleoli -0.0798761905       2 0.0537973214 0.0000000
#> 10:          1             age -0.0151575397       2 0.0140473214 0.9444444
#> 11:          1             sex -0.0010932540       2 0.0005277778       NaN
#> 12:          2     Bare.nuclei -0.0050662698       3 0.0493383117 1.0000000
#> 13:          2     Bl.cromatin  0.0610115079       3 0.0489450397 1.0000000
#> 14:          2      Cell.shape  0.1521496032       3 0.0745567280 1.0000000
#> 15:          2       Cell.size  0.1072460317       3 0.0319849206 1.0000000
#> 16:          2    Cl.thickness  0.0501396825       3 0.0262465278 1.0000000
#> 17:          2    Epith.c.size  0.0181162698       3 0.0059076389 1.0000000
#> 18:          2   Marg.adhesion  0.0858515873       3 0.0238763889 1.0000000
#> 19:          2         Mitoses  0.0172793651       3 0.0043198413 1.0000000
#> 20:          2 Normal.nucleoli  0.0572436508       3 0.0537973214 1.0000000
#> 21:          2             age -0.0293571429       3 0.0140473214 1.0000000
#> 22:          2             sex  0.0010178571       3 0.0005277778       NaN
#> 23:          3     Bare.nuclei  0.0000000000       1 0.0493383117 0.0000000
#> 24:          3     Bl.cromatin  0.0000000000       1 0.0489450397 0.5000000
#> 25:          3      Cell.shape  0.0000000000       1 0.0745567280 0.0000000
#> 26:          3       Cell.size  0.0000000000       1 0.0319849206 0.0000000
#> 27:          3    Cl.thickness  0.0002857143       1 0.0262465278 0.6666667
#> 28:          3    Epith.c.size  0.0000000000       1 0.0059076389 0.0000000
#> 29:          3   Marg.adhesion  0.0000000000       1 0.0238763889 0.0000000
#> 30:          3         Mitoses  0.0000000000       1 0.0043198413 0.0000000
#> 31:          3 Normal.nucleoli  0.0000000000       1 0.0537973214 0.0000000
#> 32:          3             age  0.0002857143       1 0.0140473214 0.0000000
#> 33:          3             sex  0.0000000000       1 0.0005277778       NaN
#> 34:          4     Bare.nuclei -0.0522933261       2 0.0493383117 0.0000000
#> 35:          4     Bl.cromatin -0.0519194444       2 0.0489450397 0.0000000
#> 36:          4      Cell.shape -0.1093753247       2 0.0745567280 0.0000000
#> 37:          4       Cell.size -0.0206936508       2 0.0319849206 0.0000000
#> 38:          4    Cl.thickness -0.0366658730       2 0.0262465278 0.0000000
#> 39:          4    Epith.c.size  0.0000000000       2 0.0059076389 0.0000000
#> 40:          4   Marg.adhesion -0.0096539683       2 0.0238763889 0.0000000
#> 41:          4         Mitoses  0.0000000000       2 0.0043198413 0.0000000
#> 42:          4 Normal.nucleoli -0.0780694444       2 0.0537973214 0.0000000
#> 43:          4             age -0.0113888889       2 0.0140473214 0.4722222
#> 44:          4             sex  0.0000000000       2 0.0005277778       NaN
#>     sample_num         feature           Phi cluster     mean_phi     f_val
#>          <int>          <char>         <num>   <int>        <num>     <num>
#>     unscaled_f_val correct_prediction    pred_prob pred_class
#>              <num>             <fctr>        <num>     <fctr>
#>  1:              1            Correct 0.0043809524     benign
#>  2:              1            Correct 0.0043809524     benign
#>  3:              1            Correct 0.0043809524     benign
#>  4:              1            Correct 0.0043809524     benign
#>  5:              4            Correct 0.0043809524     benign
#>  6:              3            Correct 0.0043809524     benign
#>  7:              1            Correct 0.0043809524     benign
#>  8:              1            Correct 0.0043809524     benign
#>  9:              1            Correct 0.0043809524     benign
#> 10:             54            Correct 0.0043809524     benign
#> 11:              2            Correct 0.0043809524     benign
#> 12:              3            Correct 0.9575134921  malignant
#> 13:              8            Correct 0.9575134921  malignant
#> 14:              7            Correct 0.9575134921  malignant
#> 15:              8            Correct 0.9575134921  malignant
#> 16:              7            Correct 0.9575134921  malignant
#> 17:              4            Correct 0.9575134921  malignant
#> 18:              6            Correct 0.9575134921  malignant
#> 19:              4            Correct 0.9575134921  malignant
#> 20:              8            Correct 0.9575134921  malignant
#> 21:             56            Correct 0.9575134921  malignant
#> 22:              2            Correct 0.9575134921  malignant
#> 23:              1            Correct 0.0005714286     benign
#> 24:              3            Correct 0.0005714286     benign
#> 25:              1            Correct 0.0005714286     benign
#> 26:              1            Correct 0.0005714286     benign
#> 27:              5            Correct 0.0005714286     benign
#> 28:              2            Correct 0.0005714286     benign
#> 29:              1            Correct 0.0005714286     benign
#> 30:              1            Correct 0.0005714286     benign
#> 31:              1            Correct 0.0005714286     benign
#> 32:             20            Correct 0.0005714286     benign
#> 33:              2            Correct 0.0005714286     benign
#> 34:              1            Correct 0.0000000000     benign
#> 35:              1            Correct 0.0000000000     benign
#> 36:              1            Correct 0.0000000000     benign
#> 37:              1            Correct 0.0000000000     benign
#> 38:              1            Correct 0.0000000000     benign
#> 39:              2            Correct 0.0000000000     benign
#> 40:              1            Correct 0.0000000000     benign
#> 41:              1            Correct 0.0000000000     benign
#> 42:              1            Correct 0.0000000000     benign
#> 43:             37            Correct 0.0000000000     benign
#> 44:              2            Correct 0.0000000000     benign
#>     unscaled_f_val correct_prediction    pred_prob pred_class
#>              <num>             <fctr>        <num>     <fctr>
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the explainer package.
#>   Please report the issue at <https://github.com/PERSIMUNE/explainer/issues>.
#> Warning: Ignoring unknown aesthetics: text
#> Warning: Arguments in `...` must be used.
#> ✖ Problematic argument:
#> • color = "black"
#> ℹ Did you misspell an argument name?
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Computation failed in `stat_ydensity()`.
#> Caused by error in `$<-.data.frame`:
#> ! replacement has 1 row, data has 0
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: Groups with fewer than two datapoints have been dropped.
#> ℹ Set `drop = FALSE` to consider such groups for position adjustment purposes.
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: Computation failed in `stat_ydensity()`.
#> Caused by error in `$<-.data.frame`:
#> ! replacement has 1 row, data has 0
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: 'ggimage' is missing. Will not plot arrows and zero-shading.
#> Warning: 'rsvg' is missing. Will not plot arrows and zero-shading.
#> Warning: 'ggimage' is missing. Will not plot arrows and zero-shading.
#> Warning: 'rsvg' is missing. Will not plot arrows and zero-shading.
#> Warning: 'ggimage' is missing. Will not plot arrows and zero-shading.
#> Warning: 'rsvg' is missing. Will not plot arrows and zero-shading.
# }
```
