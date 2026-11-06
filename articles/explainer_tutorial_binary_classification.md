# classification model interpretation using explainer package

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

This is an example on how to use the package “explainer” developed by
Ramtin Zargari Marandi (email:<ramtin.zargari@gmail.com>)

## Loading a dataset and training a machine learning model

This example uses
[`mlbench::BreastCancer`](https://rdrr.io/pkg/mlbench/man/BreastCancer.html)
to classify breast cytology samples as benign or malignant from their
recorded cell characteristics. It does not predict a person’s future
breast-cancer risk. We use only the original predictors, exclude the ID
column, and omit rows with missing values. Age and sex are not recorded
in this dataset, so we do not invent demographic values.

The first code chunk creates a binary classification task and trains a
random forest using mlr3. The original predictors are stored as factors,
so this example already demonstrates categorical feature handling
without adding synthetic patient attributes.

``` r

Sys.setenv(LANG = "en") # change R language to English!
RNGkind("L'Ecuyer-CMRG") # change to L'Ecuyer-CMRG in case it uses default "Mersenne-Twister"

library("explainer")
# set seed for reproducibility
seed <- 246
set.seed(seed)

# load the BreastCancer data from the mlbench package
data("BreastCancer", package = "mlbench")

# keep the target column as "Class"
target_col <- "Class"

# change the positive class to "malignant"
positive_class <- "malignant"
    
# keep only the predictor variables and outcome
mydata <- BreastCancer[, -1] # 1 is ID

# remove rows with missing values
mydata <- na.omit(mydata)

# create a classification task
maintask <- mlr3::TaskClassif$new(id = "my_classification_task",
                                  backend = mydata,
                                  target = target_col,
                                  positive = positive_class)

# create a train-test split
set.seed(seed)
splits <- mlr3::partition(maintask)

# add a learner (machine learning model base)
# library("mlr3learners")
# library("mlr3extralearners")

# mlr_learners$get("classif.randomForest")
# here we use random forest for example (you can use any other available model)
# mylrn <- mlr3::lrn("classif.randomForest", predict_type = "prob")
library("mlr3learners")
```

    ## Loading required package: mlr3

``` r

mylrn <- mlr3::lrn("classif.ranger", predict_type = "prob")

# train the model
mylrn$train(maintask, splits$train)

# make predictions on new data
mylrn$predict(maintask, splits$test)
```

    ## 
    ## -- <PredictionClassif> for 225 observations: -----------------------------------
    ##  row_ids     truth  response prob.malignant prob.benign
    ##        2    benign malignant    0.900477778  0.09952222
    ##        5    benign    benign    0.010041270  0.98995873
    ##        7    benign    benign    0.389238095  0.61076190
    ##      ---       ---       ---            ---         ---
    ##      671    benign    benign    0.000000000  1.00000000
    ##      675    benign    benign    0.001371429  0.99862857
    ##      681 malignant malignant    0.913784127  0.08621587

### SHAP analysis to extract feature (variable) impacts on predictions

The following code chunk uses eSHAP_plot function to estimate SHAP
values for the test set and create an interactive SHAP plot. This is an
enhanced SHAP plot that means it provides additional information such as
whether the predictions were correct (TP or TN). The color mapping
provides enhanced visual inspection of the SHAP plot.

``` r

library("magrittr")
library("plotly")
```

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r

# enhanced SHAP plot
SHAP_output <- eSHAP_plot(task = maintask,
           trained_model = mylrn,
           splits = splits,
           sample.size = 30,
           seed = seed,
           subset = .8)

# display the SHAP plot
myplot <- SHAP_output[[1]]
myplot
```

The following plot displays SHAP values associate with the predicted
probabilities.

``` r

SHAP_output[[5]]
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-3-1.png)

### Visualize model performance by confusion matrix

The following code chunk uses eCM_plot function to visualize the
confusion matrix to evaluate model performance for the train and test
sets. More information can be found here:
<https://en.wikipedia.org/wiki/Confusion_matrix>
<https://cran.r-project.org/web/packages/cvms/vignettes/Creating_a_confusion_matrix.html>

``` r

# enhanced confusion matrix
confusionmatrix_plot <- eCM_plot(task = maintask,
         trained_model = mylrn,
         splits = splits)
```

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'ggimage' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'rsvg' is missing. Will not plot arrows and zero-shading.

    ## Warning in cvms::plot_confusion_matrix(cfm, target_col = "Truth",
    ## prediction_col = "Prediction", : 'ggnewscale' is missing. Will not use palette
    ## for sum tiles.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'ggimage' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'rsvg' is missing. Will not plot arrows and zero-shading.

    ## Warning in cvms::plot_confusion_matrix(cfm, target_col = "Truth",
    ## prediction_col = "Prediction", : 'ggnewscale' is missing. Will not use palette
    ## for sum tiles.

``` r

print(confusionmatrix_plot)
```

    ## $train_set

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-4-1.png)

    ## 
    ## $test_set

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-4-2.png)

### Decision curve analysis

The provided code chunk employs the eDecisionCurve function to conduct
“decision curve analysis” on the test set within the model. For an
in-depth understanding of this methodology, interested readers are
encouraged to explore the following authoritative references:

Decision Curve Analysis:
<https://en.wikipedia.org/wiki/Decision_curve_analysis> “Decision curve
analysis: a novel method for evaluating prediction models” by Andrew J.
Vickers and Elia B. Elkin. Link:
<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2577036/> These references
offer comprehensive insights into the principles and applications of
decision curve analysis, providing a solid foundation for further
exploration and understanding of the methodology employed in the
presented code.

``` r

# enhanced decision curve plot
eDecisionCurve(task = maintask,
         trained_model = mylrn,
         splits = splits,
         seed = seed)
```

### Model evaluation (multi-metrics and visual inspection of ROC curves)

By running the next code chunk, you will get the following model
evaluation metrics and visualizations:

**AUC (Area Under the Curve):** AUC quantifies the binary classification
model’s performance by assessing the area under the ROC curve, which
plots sensitivity against 1-specificity across various threshold values.
A value of 0.5 suggests random chance performance, while 1 signifies
perfect classification.

**BACC (Balanced Accuracy):** BACC addresses class imbalance by
averaging sensitivity and specificity. Ranging from 0 to 1, a score of 0
indicates chance performance, and 1 signifies perfect classification.

**MCC (Matthews Correlation Coefficient):** MCC evaluates binary
classification model quality, considering true positives, true
negatives, false positives, and false negatives. Ranging from -1 to 1,
-1 represents complete disagreement, 0 implies chance performance, and 1
indicates perfect classification.

**BBRIER (Brier Score):** BBRIER gauges the accuracy of probabilistic
predictions by measuring the mean squared difference between predicted
probabilities and true binary outcomes. Values range from 0 to 1, with 0
indicating perfect calibration and 1 indicating poor calibration.

**PPV (Positive Predictive Value):** PPV, or precision, measures the
proportion of true positive predictions out of all positive predictions
made by the model.

**NPV (Negative Predictive Value):** NPV quantifies the proportion of
true negative predictions out of all negative predictions made by the
model.

**Specificity:** Specificity calculates the proportion of true negative
predictions out of all actual negative cases in a binary classification
problem.

**Sensitivity:** Sensitivity, also known as recall or true positive
rate, measures the proportion of true positive predictions out of all
actual positive cases in a binary classification problem.

**PRAUC (Precision-Recall Area Under the Curve):** PRAUC assesses binary
classification model performance based on precision and recall,
quantifying the area under the precision-recall curve. A PRAUC value of
1 indicates perfect classification performance.

Additionally, the analysis involves the visualization of ROC and
Precision-Recall curves for both development and test sets.

``` r

eROC_plot(task = maintask,
         trained_model = mylrn,
         splits = splits)
```

    ## [[1]]

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-6-1.png)

    ## 
    ## [[2]]
    ##             pred_results$score(measures = mlr3::msrs(meas))
    ## auc                                                    1.00
    ## bacc                                                   0.99
    ## mcc                                                    0.98
    ## bbrier                                                 0.01
    ## ppv                                                    0.98
    ## npv                                                    1.00
    ## specificity                                            0.99
    ## sensitivity                                            0.99
    ## prauc                                                  1.00
    ## 
    ## [[3]]
    ##             pred_results_test$score(measures = mlr3::msrs(meas))
    ## auc                                                         0.99
    ## bacc                                                        0.97
    ## mcc                                                         0.92
    ## bbrier                                                      0.04
    ## ppv                                                         0.93
    ## npv                                                         0.99
    ## specificity                                                 0.96
    ## sensitivity                                                 0.97
    ## prauc                                                       0.96

### ROC curves with annotated thresholds

By running the next code chunk, you will get ROC and Precision Recall
curves for the development and test sets this time with probability
threshold information.

``` r

ePerformance(task = maintask,
         trained_model = mylrn,
         splits = splits)
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## i Please use `linewidth` instead.
    ## i The deprecated feature was likely used in the explainer package.
    ##   Please report the issue at <https://github.com/PERSIMUNE/explainer/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## [[1]]

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-7-1.png)

    ## 
    ## [[2]]
    ## 
    ## [[3]]

### loading SHAP results for downstream analysis

Now we can get the outputs from eSHAP_plot function to apply clustering
on SHAP values

``` r

shap_Mean_wide <- SHAP_output[[2]]

shap_Mean_long <- SHAP_output[[3]]

shap <- SHAP_output[[4]]
```

### SHAP values in association with feature values

``` r

ShapFeaturePlot(shap_Mean_long)
```

## partial dependence of features

Partial dependence plots (PDPs): PDPs can be used to visualize the
marginal effect of a single feature on the model prediction.

``` r

ShapPartialPlot(shap_Mean_long = shap_Mean_long)
```

## extract feature values and predicted probabilities as output of the model to analyze

``` r

fval_predprob <- reshape2::dcast(shap, sample_num + pred_prob + predcorrectness ~ feature, value.var = "feature.value")
```

## Run a Shiny app to visualize 2-way partial dependence plots

``` r

library(shiny)
library(ggplot2)
# assuming your data.table is named `fval_predprob`
ui <- fluidPage(
  titlePanel("Feature Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_feature", "X-axis Feature:",
                  choices = names(fval_predprob)),
      selectInput("y_feature", "Y-axis Feature:",
                  choices = names(fval_predprob)),
      width = 2,
      actionButton("stop_button", "Stop")
    ),
    mainPanel(
      plotOutput("feature_plot")
    )
  )
)

server <- function(input, output) {
  
  # create a reactive value for the app state
  app_state <- reactiveValues(running = TRUE)
  
  output$feature_plot <- renderPlot({
    ggplot(fval_predprob, aes(x = .data[[input$x_feature]], y = .data[[input$y_feature]], fill = pred_prob)) + 
      geom_tile() +
      scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
      xlab(input$x_feature) +
      ylab(input$y_feature) +
      labs(shape = "correct prediction") +
      egg::theme_article()
  })
  
  # observe the stop button
  observeEvent(input$stop_button, {
    app_state$running <- FALSE
  })
  
  # stop the app if the running state is FALSE
  observe({
    if(!app_state$running) {
      stopApp()
    }
  })
}

shinyApp(ui = ui, server = server)
```

``` r

str(shap_Mean_long)
```

    ## tibble [1,620 x 9] (S3: tbl_df/tbl/data.frame)
    ##  $ feature           : chr [1:1620] "Bare.nuclei" "Bare.nuclei" "Bare.nuclei" "Bare.nuclei" ...
    ##  $ mean_phi          : num [1:1620] 0.0988 0.0988 0.0988 0.0988 0.0988 ...
    ##  $ Phi               : num [1:1620] -0.0764 -0.0623 -0.0567 0.0884 0.0713 ...
    ##  $ f_val             : num [1:1620] 0 0.111 0 0.444 0.556 ...
    ##  $ unscaled_f_val    : num [1:1620] 1 2 1 5 6 1 1 3 1 1 ...
    ##  $ sample_num        : int [1:1620] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ correct_prediction: Factor w/ 2 levels "Incorrect","Correct": 2 2 2 1 2 2 2 2 2 2 ...
    ##  $ pred_prob         : num [1:1620] 0.00351 0.00387 0 0.93658 0.98563 ...
    ##  $ pred_class        : Factor w/ 2 levels "malignant","benign": 2 2 2 1 1 2 2 1 2 2 ...

``` r

# This is the data table that includes SHAP values in wide format. Each row contains sample_num as sample ID and the SHAP values for each feature.
shap_Mean_wide
```

    ## Key: <sample_num>
    ##      sample_num Bare.nuclei Bl.cromatin  Cell.shape   Cell.size Cl.thickness
    ##           <int>       <num>       <num>       <num>       <num>        <num>
    ##   1:          1 -0.07639570 -0.05706958 -0.06619589 -0.05223212  0.002059577
    ##   2:          2 -0.06231476 -0.03123529 -0.05198693 -0.05631366 -0.048326878
    ##   3:          3 -0.05666370 -0.01267601 -0.03782910 -0.04817529 -0.034655370
    ##   4:          4  0.08837070  0.06956929  0.08884785  0.08741011 -0.023663571
    ##   5:          5  0.07133293  0.07332603  0.08644455  0.12104929 -0.008073228
    ##  ---                                                                        
    ## 176:        176 -0.05850143 -0.03343622 -0.09099357 -0.12194661 -0.070548148
    ## 177:        177 -0.06186537 -0.04724013 -0.06136190 -0.07985424 -0.086679365
    ## 178:        178 -0.07486704 -0.02945307 -0.07541830 -0.06231672 -0.051375582
    ## 179:        179 -0.14022320  0.03093902 -0.05861410 -0.06368788 -0.042006323
    ## 180:        180  0.25993000  0.01606952  0.14525574  0.12395902  0.139356878
    ##      Epith.c.size Marg.adhesion       Mitoses Normal.nucleoli
    ##             <num>         <num>         <num>           <num>
    ##   1: -0.017461746 -0.0055123810 -7.301587e-05    -0.033975238
    ##   2: -0.006284735 -0.0150961111 -9.077778e-04    -0.014735317
    ##   3: -0.012663307 -0.0185450265 -1.155714e-03    -0.009536217
    ##   4:  0.051964839  0.0427648677 -1.917725e-04     0.008738413
    ##   5:  0.032355529  0.0455281217  1.385000e-03     0.006960291
    ##  ---                                                         
    ## 176: -0.017221720 -0.0299588095 -2.251720e-03    -0.026591984
    ## 177: -0.020393516 -0.0156755026 -2.188175e-03    -0.016636376
    ## 178: -0.009410342 -0.0168008466 -7.193386e-04    -0.017697407
    ## 179: -0.059117963 -0.0003837037 -1.479894e-03    -0.009989524
    ## 180: -0.005184074  0.0648683862  2.909524e-03     0.029994206

``` r

# This is the data table of SHAP values in long format and includes feature name, mean_phi_test: mean shap value for the feature across samples, scaled feature values, shap values for each feature, sample ID, and whether the prediction was correct (i.e. predicted class = actual class)
shap_Mean_long
```

    ## # A tibble: 1,620 x 9
    ##    feature   mean_phi     Phi f_val unscaled_f_val sample_num correct_prediction
    ##    <chr>        <dbl>   <dbl> <dbl>          <dbl>      <int> <fct>             
    ##  1 Bare.nuc~   0.0988 -0.0764 0                  1          1 Correct           
    ##  2 Bare.nuc~   0.0988 -0.0623 0.111              2          2 Correct           
    ##  3 Bare.nuc~   0.0988 -0.0567 0                  1          3 Correct           
    ##  4 Bare.nuc~   0.0988  0.0884 0.444              5          4 Incorrect         
    ##  5 Bare.nuc~   0.0988  0.0713 0.556              6          5 Correct           
    ##  6 Bare.nuc~   0.0988 -0.0536 0                  1          6 Correct           
    ##  7 Bare.nuc~   0.0988 -0.0547 0                  1          7 Correct           
    ##  8 Bare.nuc~   0.0988 -0.0369 0.222              3          8 Correct           
    ##  9 Bare.nuc~   0.0988 -0.0848 0                  1          9 Correct           
    ## 10 Bare.nuc~   0.0988 -0.102  0                  1         10 Correct           
    ## # i 1,610 more rows
    ## # i 2 more variables: pred_prob <dbl>, pred_class <fct>

## Patient subgroups determined by SHAP clusters

SHAP clustering is a method to better understand why a model may perform
better for some patients than others. Here for example, you can identify
patient subgroups that have specific patterns that are different from
other subgroups and that explains why the model you developed have
perhaps better or worse performance for those patients than the average
performance for the whole dataset. You can see the difference in the
SHAP plots that if you group all together provide the overall SHAP
summary plot. Again here the edges reflect how features may interact
with each other in each individual sample (instance).

``` r

# the number of clusters can be changed
SHAP_plot_clusters <- SHAPclust(task = maintask,
         trained_model = mylrn,
         splits = splits,
         shap_Mean_wide = shap_Mean_wide,
         shap_Mean_long = shap_Mean_long,
         num_of_clusters = 3,
         seed = seed,
         subset = .8)
```

    ## Key: <sample_num, feature, Phi>
    ##       sample_num         feature          Phi cluster    mean_phi     f_val
    ##            <int>          <char>        <num>   <int>       <num>     <num>
    ##    1:          1     Bare.nuclei -0.076395697       1 0.098771160 0.0000000
    ##    2:          1     Bl.cromatin -0.057069577       1 0.045911901 0.0000000
    ##    3:          1      Cell.shape -0.066195892       1 0.080299347 0.0000000
    ##    4:          1       Cell.size -0.052232121       1 0.091891452 0.0000000
    ##    5:          1    Cl.thickness  0.002059577       1 0.070565158 0.5000000
    ##   ---                                                                      
    ## 1616:        180    Cl.thickness  0.139356878       2 0.070565158 0.7500000
    ## 1617:        180    Epith.c.size -0.005184074       2 0.023117513 0.1111111
    ## 1618:        180   Marg.adhesion  0.064868386       2 0.028259135 1.0000000
    ## 1619:        180         Mitoses  0.002909524       2 0.001921332 0.1250000
    ## 1620:        180 Normal.nucleoli  0.029994206       2 0.016990259 0.7777778
    ##       unscaled_f_val correct_prediction   pred_prob pred_class
    ##                <num>             <fctr>       <num>     <fctr>
    ##    1:              1            Correct 0.003514286     benign
    ##    2:              1            Correct 0.003514286     benign
    ##    3:              1            Correct 0.003514286     benign
    ##    4:              1            Correct 0.003514286     benign
    ##    5:              5            Correct 0.003514286     benign
    ##   ---                                                         
    ## 1616:              7            Correct 0.962276984  malignant
    ## 1617:              2            Correct 0.962276984  malignant
    ## 1618:             10            Correct 0.962276984  malignant
    ## 1619:              2            Correct 0.962276984  malignant
    ## 1620:              8            Correct 0.962276984  malignant

    ## Warning in geom_jitter(aes(shape = correct_prediction, text = paste("Feature:
    ## ", : Ignoring unknown aesthetics: text

    ## Warning in ggplot2::guide_legend(color = "black"): Arguments in `...` must be used.
    ## x Problematic argument:
    ## * color = "black"
    ## i Did you misspell an argument name?

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'ggimage' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'rsvg' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'ggimage' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'rsvg' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'ggimage' is missing. Will not plot arrows and zero-shading.

    ## Warning in check_gg_image_packages(add_arrows = add_arrows, add_zero_shading =
    ## add_zero_shading): 'rsvg' is missing. Will not plot arrows and zero-shading.

``` r

# note that the subset must be the same value as the SHAP analysis done earlier

# display the SHAP cluster plots
SHAP_plot_clusters[[1]]
```

``` r

# display the confusion matrices corresponding to the SHAP clusters (patient subsets determined by SHAP clusters)
SHAP_plot_clusters[[2]]
```

![](explainer_tutorial_binary_classification_files/figure-html/unnamed-chunk-17-1.png)

## Subgroup performance and model fairness

[`eFairness()`](https://persimune.github.io/explainer/reference/eFairness.md)
compares model performance across levels of a recorded categorical
variable. This dataset does not include demographic attributes such as
age or sex, so it cannot support a demographic fairness analysis.
Randomly assigning those attributes would create arbitrary groups and
would not provide evidence about fairness for real patients.

For a fairness analysis on your own data, use a genuinely recorded
subgroup variable, pass its actual factor levels to `var_levels`, and
check that each subgroup contains enough observations from both outcome
classes. Differences across cell-characteristic categories can describe
subgroup performance, but should not be interpreted as demographic
fairness.

### Model parameters

``` r

# get model parameters
model_params <- mylrn$param_set

print(data.table::as.data.table(model_params))
```

    ##                               id    class lower upper
    ##                           <char>   <char> <num> <num>
    ##  1:       always.split.variables ParamUty    NA    NA
    ##  2:                class.weights ParamUty    NA    NA
    ##  3:                      holdout ParamLgl    NA    NA
    ##  4:                   importance ParamFct    NA    NA
    ##  5:                   keep.inbag ParamLgl    NA    NA
    ##  6:                    max.depth ParamInt     1   Inf
    ##  7:                   min.bucket ParamUty    NA    NA
    ##  8:                min.node.size ParamUty    NA    NA
    ##  9:                         mtry ParamInt     1   Inf
    ## 10:                   mtry.ratio ParamDbl     0     1
    ## 11:                    na.action ParamFct    NA    NA
    ## 12:            num.random.splits ParamInt     1   Inf
    ## 13:                   node.stats ParamLgl    NA    NA
    ## 14:                  num.threads ParamInt     1   Inf
    ## 15:                    num.trees ParamInt     1   Inf
    ## 16:                    oob.error ParamLgl    NA    NA
    ## 17:        regularization.factor ParamUty    NA    NA
    ## 18:      regularization.usedepth ParamLgl    NA    NA
    ## 19:                      replace ParamLgl    NA    NA
    ## 20:    respect.unordered.factors ParamFct    NA    NA
    ## 21:              sample.fraction ParamDbl     0     1
    ## 22:                  save.memory ParamLgl    NA    NA
    ## 23: scale.permutation.importance ParamLgl    NA    NA
    ## 24:             local.importance ParamLgl    NA    NA
    ## 25:                         seed ParamInt  -Inf   Inf
    ## 26:         split.select.weights ParamUty    NA    NA
    ## 27:                    splitrule ParamFct    NA    NA
    ## 28:                      verbose ParamLgl    NA    NA
    ## 29:                 write.forest ParamLgl    NA    NA
    ##                               id    class lower upper
    ##                           <char>   <char> <num> <num>
    ##                                           levels nlevels is_bounded
    ##                                           <list>   <num>     <lgcl>
    ##  1:                                       [NULL]     Inf      FALSE
    ##  2:                                       [NULL]     Inf      FALSE
    ##  3:                                   TRUE,FALSE       2       TRUE
    ##  4: none,impurity,impurity_corrected,permutation       4       TRUE
    ##  5:                                   TRUE,FALSE       2       TRUE
    ##  6:                                       [NULL]     Inf      FALSE
    ##  7:                                       [NULL]     Inf      FALSE
    ##  8:                                       [NULL]     Inf      FALSE
    ##  9:                                       [NULL]     Inf      FALSE
    ## 10:                                       [NULL]     Inf       TRUE
    ## 11:                     na.learn,na.omit,na.fail       3       TRUE
    ## 12:                                       [NULL]     Inf      FALSE
    ## 13:                                   TRUE,FALSE       2       TRUE
    ## 14:                                       [NULL]     Inf      FALSE
    ## 15:                                       [NULL]     Inf      FALSE
    ## 16:                                   TRUE,FALSE       2       TRUE
    ## 17:                                       [NULL]     Inf      FALSE
    ## 18:                                   TRUE,FALSE       2       TRUE
    ## 19:                                   TRUE,FALSE       2       TRUE
    ## 20:                       ignore,order,partition       3       TRUE
    ## 21:                                       [NULL]     Inf       TRUE
    ## 22:                                   TRUE,FALSE       2       TRUE
    ## 23:                                   TRUE,FALSE       2       TRUE
    ## 24:                                   TRUE,FALSE       2       TRUE
    ## 25:                                       [NULL]     Inf      FALSE
    ## 26:                                       [NULL]     Inf      FALSE
    ## 27:                    gini,extratrees,hellinger       3       TRUE
    ## 28:                                   TRUE,FALSE       2       TRUE
    ## 29:                                   TRUE,FALSE       2       TRUE
    ##                                           levels nlevels is_bounded
    ##                                           <list>   <num>     <lgcl>
    ##     special_vals        default storage_type                   tags
    ##           <list>         <list>       <char>                 <list>
    ##  1:    <list[0]> <NoDefault[0]>         list                  train
    ##  2:    <list[0]>         [NULL]         list                  train
    ##  3:    <list[0]>          FALSE      logical                  train
    ##  4:    <list[0]> <NoDefault[0]>    character                  train
    ##  5:    <list[0]>          FALSE      logical                  train
    ##  6:    <list[1]>         [NULL]      integer                  train
    ##  7:    <list[0]>              1         list                  train
    ##  8:    <list[1]>         [NULL]         list                  train
    ##  9:    <list[1]> <NoDefault[0]>      integer                  train
    ## 10:    <list[0]> <NoDefault[0]>      numeric                  train
    ## 11:    <list[0]>       na.learn    character                  train
    ## 12:    <list[0]>              1      integer                  train
    ## 13:    <list[0]>          FALSE      logical                  train
    ## 14:    <list[0]>              1      integer  train,predict,threads
    ## 15:    <list[0]>            500      integer train,predict,hotstart
    ## 16:    <list[0]>           TRUE      logical                  train
    ## 17:    <list[0]>              1         list                  train
    ## 18:    <list[0]>          FALSE      logical                  train
    ## 19:    <list[0]>           TRUE      logical                  train
    ## 20:    <list[0]> <NoDefault[0]>    character                  train
    ## 21:    <list[0]> <NoDefault[0]>      numeric                  train
    ## 22:    <list[0]>          FALSE      logical                  train
    ## 23:    <list[0]>          FALSE      logical                  train
    ## 24:    <list[0]>          FALSE      logical                  train
    ## 25:    <list[1]>         [NULL]      integer          train,predict
    ## 26:    <list[0]>         [NULL]         list                  train
    ## 27:    <list[0]>           gini    character                  train
    ## 28:    <list[0]>           TRUE      logical          train,predict
    ## 29:    <list[0]>           TRUE      logical                  train
    ##     special_vals        default storage_type                   tags
    ##           <list>         <list>       <char>                 <list>
