# regression model interpretation using explainer package

``` r

Sys.setenv(LANG = "en") # change R language to English!
RNGkind("L'Ecuyer-CMRG") # change to L'Ecuyer-CMRG in case it uses default "Mersenne-Twister"

library("explainer")
# library("magrittr")
# Set seed for reproducibility
seed <- 246
set.seed(seed)

# Load the BreastCancer data from the mlbench package
data("BreastCancer", package = "mlbench")

# Keep only the predictor variables and outcome
mydata <- BreastCancer[, -1] # 1 is ID

# Remove rows with missing values
mydata <- na.omit(mydata)

# Create a vector of sex categories
sex <- sample(c("Male", "Female"), size = nrow(mydata), replace = TRUE)

# Create a vector of age categories (instead of a continuous variable in the previous code)
mydata$age <- sample(seq(18, 60), size = nrow(mydata), replace = TRUE)

# Add a sex column to the mydata data frame (for fairness analysis)
mydata$sex <- factor(sex, levels = c("Male", "Female"), labels = c(1, 0))

mydata$Class <- NULL
mydata$Cl.thickness <- as.numeric(mydata$Cl.thickness)
# Keep the target column as "Class"
target_col <- "Cl.thickness"


# Create a regression task
maintask <- mlr3::TaskRegr$new(id = "my_regression_task",
                               backend = mydata,
                               target = target_col)

# Create a train-test split
set.seed(seed)
splits <- mlr3::partition(maintask)

# Add a learner (machine learning model base)
# library("mlr3learners")
# library("mlr3extralearners")

# mlr_learners$get("regr.randomForest")
# Here we use random forest for example (you can use any other available model)
# mylrn <- mlr3::lrn("regr.randomForest", predict_type = "response")
library("mlr3learners")
```

    ## Loading required package: mlr3

``` r

mylrn <- mlr3::lrn("regr.ranger", predict_type = "response")
# Train the model
mylrn$train(maintask, splits$train)

# Make predictions on new data
reg_model_outputs <- mylrn$predict(maintask, splits$test)
head(data.table::as.data.table(reg_model_outputs))
```

    ##    row_ids truth response
    ##      <int> <num>    <num>
    ## 1:       2     5 6.501367
    ## 2:       5     4 3.226313
    ## 3:       7     1 3.592489
    ## 4:      15     8 6.682733
    ## 5:      16     7 5.530967
    ## 6:      17     4 2.710123

``` r

# enhanced SHAP plot
SHAP_output <- eSHAP_plot_reg(task = maintask,
           trained_model = mylrn,
           splits = splits,
           sample.size = 30,
           seed = seed,
           subset = .8)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## i Please use `linewidth` instead.
    ## i The deprecated feature was likely used in the explainer package.
    ##   Please report the issue at <https://github.com/PERSIMUNE/explainer/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning in geom_jitter(aes(text = paste("Feature: ", feature, "<br>Unscaled
    ## feature value: ", : Ignoring unknown aesthetics: text

``` r

# display the SHAP plot
SHAP_output[[1]]
```

### regression model evaluation

``` r

# Regression model evaluation
regressmdl_eval_results <- regressmdl_eval(task = maintask,
           trained_model = mylrn,
           splits = splits)

# display the results
regressmdl_eval_results
```

    ##        MSE     RMSE      MAE R_squared
    ## 1 4.393614 2.096095 1.777186 0.4177224

Mean Squared Error (MSE): Measures the average of the squared
differences between predicted and actual values. Smaller MSE values
indicate better model performance, but it is sensitive to outliers.

Root Mean Squared Error (RMSE): The square root of MSE, providing a
measure of the average absolute errors. Lower RMSE values signify better
model accuracy.

Mean Absolute Error (MAE): Calculates the average of the absolute
differences between predicted and actual values. MAE is less sensitive
to outliers than MSE and RMSE.

R-squared (R²): Represents the proportion of the variance in the target
variable that the model explains. R² ranges from 0 to 1, where higher
values indicate a better fit to the data.

When evaluating a regression model, aim to minimize MSE, RMSE, and MAE
while maximizing R-squared to achieve the most accurate and precise
predictions. A combination of these metrics provides a comprehensive
assessment of the model’s performance and suitability for the given
task.
