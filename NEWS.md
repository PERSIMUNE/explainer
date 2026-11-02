# explainer 1.0.3

* Update the maintainer contact address in response to CRAN's bounced-email report (#4).
* Remove the unused ggpmisc dependency to resolve a CRAN check NOTE.
* Qualify SHAP helper calls so they resolve correctly when the package is used without attaching dplyr.
* Synchronize the documented confusion-matrix palette default with the function.

# explainer 1.0.2

* Add multi-class classification support with `eSHAP_plot_multiclass()` and a tutorial demonstrating SHAP analysis for multi-class models.
