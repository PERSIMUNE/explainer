# ExplaineR

<img src="man/figures/logo.png" align="right"/>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10201287.svg)](https://doi.org/10.5281/zenodo.10201287) [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/explainer)](https://cran.r-project.org/package=explainer) [![Downloads](http://cranlogs.r-pkg.org/badges/explainer)](https://cran.r-project.org/package=explainer)

## Overview

It enables detailed interpretation of complex classification and regression models through Shapley analysis including data-driven characterization of subgroups of individuals. Furthermore, it facilitates multi-measure model evaluation, model fairness, and decision curve analysis. Additionally, it offers enhanced visualizations with interactive elements. Find out how to use the package by checking out the package tutorials.

## Installation

You can install the released version of the package from CRAN:

``` r
install.packages("explainer")
```

And the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("PERSIMUNE/explainer")
```

## Usage

Here is a short example on how to use the package. See articles for more examples.

``` r
# Load necessary packages
library("explainer")

# Set seed for reproducibility
seed <- 246
set.seed(seed)

# load necessary packages
library(mlbench)
library(mlr3learners)
library(ranger)

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
  num_of_clusters = 3,
  seed = seed,
  subset = 0.8
)

# Please note that the colors on the legend on SHAP cluster plot is not the same as in the SHAP summary plot, but the markers are the same.
# The markers indicate prediction correctness not the colors (so the colors could be manually changed to black/white on the legend when reporting)
```

## Documentation

-   [Official Documentation](https://persimune.github.io/explainer/)


## Citation

If you use this package in your research, please consider citing it:

Zargari Marandi, Ramtin. "ExplaineR: an R package to explain machine learning models." Bioinformatics Advances 4, no. 1 (2024): vbae049.[doi:10.1093/bioadv/vbae049](https://doi.org/10.1093/bioadv/vbae049)

## Contributing

If you want to contribute to the development of this package, please read [CONTRIBUTING.md](https://github.com/PERSIMUNE/explainer/blob/main/.github/CONTRIBUTING.md) for guidelines.

## Issues

If you encounter any issues or have suggestions, please [open an issue](https://github.com/PERSIMUNE/explainer/issues).

## License

This package is released under the [MIT License](https://github.com/PERSIMUNE/explainer/blob/main/LICENSE.md).

## References

Zargari Marandi R, Leung P, Sigera C, Murray DD, Weeratunga P, Fernando D, Rodrigo C, Rajapakse S, MacPherson CR, (2023). Development of a machine learning model for early prediction of plasma leakage in suspected dengue patients. PLoS Negl Trop Dis 17(3): e0010758. [doi:10.1371/journal.pntd.0010758](https://doi.org/10.1371/journal.pntd.0010758)

Lang M, Binder M, Richter J, Schratz P, Pfisterer F, Coors S, Au Q, Casalicchio G, Kotthoff L, Bischl B, (2019). mlr3: A modern object-oriented machine learning framework in R. Journal of Open Source Software. [doi:10.21105/joss.01903](https://joss.theoj.org/papers/10.21105/joss.01903)

Molnar C, Bischl B, Casalicchio G, (2018). iml: An R package for Interpretable Machine Learning.JOSS, 3(26), 786. [doi:10.21105/joss.00786](https://doi.org/10.21105/joss.00786)

Lundberg SM and Lee SI, (2017). A unified approach to interpreting model predictions. Advances in neural information processing systems.[arXiv:1705.07874](https://arxiv.org/abs/1705.07874)

Ludvig Renbo Olsen and Hugh Benjamin Zachariae, (2022). cvms: Cross-Validation for Model Selection. R package version 1.3.4. <https://CRAN.R-project.org/package=cvms>

R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.

## Acknowledgements

Many thanks to my colleagues at CHIP/PERSIMUNE to support this work by their feedback. In addition, thanks to the CRAN team as well as the authors and maintainers of all the utilized packages for their dedication to advancing the R ecosystem.

## Funding

This work was supported by the Danish National Research Foundation (DNRF126).
