# jamovi-DiagnosticTest

`DiagnosticTest` provides statistical analyses to evaluate a diagnostic
test in [jamovi](https://www.jamovi.org), based on clinical trials results. 
In this vesion, only the ROC Analysis for Continuous Predictors is available, 
which is based on the [pROC package](https://cran.r-project.org/web/packages/pROC/index.html).

## Installation in jamovi

You can install `DiagnosticTest` by downloading the `.jmo` file available in 
this repository and side-loading it directly on the [jamovi](https://www.jamovi.org)
interface.

## Dataset and variables used

For the ROC Analysis, you will need a dataset with at least two variables: 
one containing the continuous predictor, and another containing the binary 
outcome coded as 1 for cases and 0 for controls.

## About the this version

`25 March 2025`: "In the current version, you can easily generate ROC curves 
and view their local maxima displayed in a cut-off table. Future developments 
will include the addition of new statistics to the current analysis, enhancements 
to the user experience through new options and customizations, and, eventually, 
the implementation of new types of analyses.
