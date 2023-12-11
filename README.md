# ExplaineR

<img src="man/figures/logo.png" align="right" />

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10201287.svg)](https://doi.org/10.5281/zenodo.10201287)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/explainer)](https://cran.r-project.org/package=explainer)
[![Downloads](http://cranlogs.r-pkg.org/badges/explainer)](https://cran.r-project.org/package=explainer)

## Overview

It enables detailed interpretation of complex classification and regression models through Shapley analysis including data-driven characterization of subgroups of individuals. Furthermore, it facilitates multi-measure model evaluation, model fairness, and decision curve analysis. Additionally, it offers enhanced visualizations with interactive elements. Find out how to use the package by checking out the package tutorials.

## Installation

You can install the released version of the package from CRAN:

```R
install.packages("explainer")
```

And the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("PERSIMUNE/explainer")
```

## Usage

Provide examples of how to use your package. This could include basic code snippets or links to more detailed vignettes and documentation.

```R
# Load necessary packages
library("explainer")

# Set seed for reproducibility
seed <- 246
set.seed(seed)

# Install and load necessary packages if not already installed
if (!requireNamespace("mlbench", quietly = TRUE)) {
  install.packages("mlbench")
  library(mlbench)
}
if (!requireNamespace("mlr3learners", quietly = TRUE)) {
  install.packages("mlr3learners")
  library(mlr3learners)
}
if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger")
  library(ranger)
}

# Load BreastCancer dataset
utils::data("BreastCancer", package = "mlbench")

# Specify target column and positive class
target_col <- "Class"
positive_class <- "malignant"

# Extract relevant columns from dataset
mydata <- BreastCancer[, -1]
mydata <- na.omit(mydata)

# Generate random 'sex' and 'age' columns
sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)
mydata$age <- as.numeric(sample(seq(18, 60), size = nrow(mydata), replace = TRUE))
mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))

# Create a classification task
maintask <- mlr3::TaskClassif$new(
  id = "my_classification_task",
  backend = mydata,
  target = target_col,
  positive = positive_class
)

# Split the dataset for training
splits <- mlr3::partition(maintask)

# Create a ranger learner for classification
mylrn <- mlr3::lrn("classif.ranger", predict_type = "prob")

# Train the learner on the training set
mylrn$train(maintask, splits$train)

# Generate SHAP values and plot
SHAP_output <- eSHAP_plot(
  task = maintask,
  trained_model = mylrn,
  splits = splits,
  sample.size = 30,
  seed = seed,
  subset = 0.8
)

shap_Mean_wide <- SHAP_output[[2]]

shap_Mean_long <- SHAP_output[[3]]

# Generate SHAP clusters and plot
SHAP_plot_clusters <- SHAPclust(
  task = maintask,
  trained_model = mylrn,
  splits = splits,
  shap_Mean_wide = shap_Mean_wide,
  shap_Mean_long = shap_Mean_long,
  num_of_clusters = 4,
  seed = seed,
  subset = 0.8
)


```

## Documentation

- [Official Documentation](https://persimune.github.io/explainer/)


## Contributing

If you want to contribute to the development of this package, please read [CONTRIBUTING.md](https://github.com/PERSIMUNE/explainer/blob/main/.github/CONTRIBUTING.md) for guidelines.

## Issues

If you encounter any issues or have suggestions, please [open an issue](https://github.com/PERSIMUNE/explainer/issues).

## License

This package is released under the [MIT License](https://github.com/PERSIMUNE/explainer/blob/main/LICENSE.md).

## Citation

If you use this package in your research, please consider citing it:

```
citation("explainer")
```
## References

Zargari Marandi R, Leung P, Sigera C, Murray DD, Weeratunga P, Fernando D, et al. (2023) Development of a machine learning model for early prediction of plasma leakage in suspected dengue patients. PLoS Negl Trop Dis 17(3): e0010758. https://doi.org/10.1371/journal.pntd.0010758
