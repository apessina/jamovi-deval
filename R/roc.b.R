
# This script is the core of the ROC Analysis using pROC R library. It uses one
# continuous variable and one outcome variable from the UI to generate the ROC
# curve, calculate AUC and the local maximas table. The ROC plot is printed.
# Last edit: 26 March 2025; PESSINA, A. L. R.

# Define the R6Class for the module
ROCClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ROCClass",
  inherit = ROCBase,
  private = list(
    
    # Create the .run function for data, text and tables
    .run = function() {
      
      ##############################################
      ##### Defining the data for the analysis #####
      ##############################################
      
      ## Check if the UI variables are defined
      if (is.null(self$options$varPred) || is.null(self$options$varOutc))
        return()
      
      ## Get the data
      data <- self$data # get the UI dataframe
      data <- na.omit(data) # remove NAs
      
      ## Check if the outcome variable is binary (0 and 1)
      if (length(levels(as.factor(data[[self$options$varOutc]]))) != 2) {
        self$results$text$setContent("ATTENTION! The case (1) / control (0) variable must have 2 levels.")
        return()
      }
      
      ## Select our interesting data
      predictors <- data[[self$options$varPred]] # continuous
      responses <- data[[self$options$varOutc]] # binary
      
      ##############################################################
      ##### Making the calculations and presenting the results #####
      ##############################################################
      
      ##  Calculate the ROC Curve using pROC library
      roc_obj <- pROC::roc(responses, predictors)
      
      ## Calculate the Area Under the Curve with confidence interval
      auc_ci <- pROC::ci.auc(roc_obj, conf.level=self$options$DeLongCl/100, 
                             method="delong") # CI with DeLong's variance
      
      ## Fill the main results one-line table
      tableMain <- self$results$tableMain
      tableMain$setRow(rowNo=1, values=list(
        var=self$options$varPred, # name of the predictor variable
        AUC=round(auc_ci[2], 3), # AUC value of the calculated curve
        CI=sprintf("%d%%: %.3f–%.3f", # ex. 95%: 0.678-0.789 (DeLong)
                   round(self$options$DeLongCl), auc_ci[1], auc_ci[3])
      ))
      
      ## Get the local maximas
      roc_coords <- pROC::coords(roc_obj, x="local maximas", ret="all")
      
      ## Fill the cut-offs table
      tableCo <- self$results$tableCo
      tableCo$setTitle(self$options$varPred) # title: predictor name
      for (i in seq_along(roc_coords$threshold)) { # one new row per cut-off
        tableCo$addRow(rowKey=i, values=list(
          Cutoff=roc_coords$threshold[i], # cut-off 
          TPR=roc_coords$tpr[i], # sensitivity of the cut-off 
          FPR=roc_coords$fpr[i], # 1-specificity of the cut-off 
          TNR=roc_coords$tnr[i], # specificity of the cut-off 
          FNR=roc_coords$fnr[i], # 1-sensitivity  of the cut-off 
          Topleft=roc_coords$closest.topleft[i], # top-left dist. of the cut-off 
          Youden=roc_coords$youden[i] # youden´s J of the cut-off 
        ))          
      }
      
      #######################################
      ##### Creating the ROC Curve plot #####
      #######################################
      
      ## Set-up the plot object with the plot data in a dataframe
      image <- self$results$plot
      image$setState(data.frame(responses=responses, predictors=predictors))
      
    }, # close the .run function
    
    # Create the .plot function
    .plot = function(image, ...) {
      
      ## Check if there is data for the plot
      if (is.null(image$state))
        return(FALSE)
      
      ## Deactivate the default box around the plotting area
      par(bty="n")
      
      ## Create and set-up the ROC Curve plot using the pROC library
      pROC::plot.roc(
        image$state$responses, image$state$predictors, # variables
        percent=TRUE, # show the sensitivity and specificity as percentages
        legacy.axes=TRUE, # increasing specificity axis (1-Specificity)
        main=self$options$varPred, # title: predictor name
        print.thres="local maximas", print.thres.pattern="%.2f", # cut-offs
        print.auc=TRUE, print.auc.x=40, print.auc.y=10, # auc position
        identity.col="red", identity.lty=2 # red dashed discrimination line
      )
      
      ## Notify the rendering system that we have plotted something
      TRUE
      
    } # close the .plot function
    
  ) # close the list
) # and close the R6Class
