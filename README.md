# jamovi-DiagnosticTest

`DiagnosticTest` provides statistical analyses to evaluate a diagnostic
test in [jamovi](https://www.jamovi.org), based on clinical trials results. 
In this version, only the ROC Analysis for Continuous Predictors is available, 
which is based mainly on the [pROC package](https://cran.r-project.org/web/packages/pROC/index.html).

## Installation in jamovi

You can install `DiagnosticTest` by downloading the `.jmo` file available in 
this repository and side-loading it directly on the [jamovi](https://www.jamovi.org)
interface.

## Dataset and variables

For the ROC Analysis, you will need a dataset with at least two variables: 
one containing the continuous predictor, and another containing the binary 
outcome coded as 1 for cases and 0 for controls.

## About last versions

`0.1.2` Added DeLong's z-test option with H₁: AUC ≠ 0.5, added dynamic note for 
cut-offs table informing the direction of the ROC curve and changed the plot, 
now with decimal scale for axis instead of percentage. Variable title now explicitly 
inform 1 for cases and 0 for controls.

`0.1.1` Added the confidence interval for AUC with custom confidence level, new 
metrics in local maximas table and changed the plot x-axis to increasing 1-Specificity. 
Changes in code for clarity and coherence. Titles were changed for the UI in
variables and tables. Added suggestion and restrictions in variables fields.

`0.1.0` In the current version, you can easily generate ROC curves 
and view their local maxima displayed in a cut-off table. Future developments 
will include the addition of new statistics to the current analysis, enhancements 
to the user experience through new options and customizations, and, eventually, 
the implementation of new types of analyses.
