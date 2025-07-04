
# This script is the core of the ROC Analysis using pROC R library. It uses one
# continuous variable and one outcome variable from the UI to generate the ROC
# curve, calculate AUC and the local maximas table. The ROC plot is printed.
# Last edit: 05 April 2025; AP.

# Define the R6Class for the module
rocClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "rocClass",
  inherit = rocBase,
  private = list(
    
    # Create the .run function for data, text and tables
    .run = function() {
      
      ##### User Data #####
      
      ## Check if the UI variables are defined
      if (is.null(self$options$pred) || is.null(self$options$varOutc))
        return()
      
      ## Get the data
      data <- self$data # get the UI dataframe
      data <- na.omit(data) # remove NAs

      ## Select our interesting data
      predictors <- data[[self$options$pred]] # continuous
      responses <- data[[self$options$varOutc]] # binary
      
      ## Check if the outcome variable is binary, with only 0 and 1
      if (any(responses!=0 & responses!=1))
        stop('Inform only 1 for Cases and 0 for Controls (healthy)')
      
      ##### Calculations #####
      
      ##  Calculate the ROC Curve using pROC library
      roc_obj <- pROC::roc(responses, predictors)
      
      ## Calculate the Area Under the Curve with confidence interval
      auc_ci <- pROC::ci.auc(roc_obj, conf.level=self$options$ciWidth/100, 
                             method="delong") # CI with DeLong's variance
      
      ## Calculate DeLong's Z with H₁: AUC > 0.5
      auc_se <- sqrt(pROC::var(roc_obj, method="delong")) # standard error
      z <- (pROC::auc(roc_obj) - 0.5) / auc_se # Z statistics
      p <- 1 - pnorm(z) # unilateral p-value
      
      ##### Results ####
      
      ## Fill the main results one-line table
      tableMain <- self$results$tableMain
      
      ciText <- paste0(self$options$ciWidth, "% Confidence Interval")
      tableMain$getColumn('cil')$setSuperTitle(ciText)
      tableMain$getColumn('ciu')$setSuperTitle(ciText)
      
      tableMain$setRow(rowNo=1, values=list(
        auc=format(round(auc_ci[2], 3), nsmall=3), # AUC 
        cil=format(round(auc_ci[1], 3), nsmall=3), # Lower AUC CI
        ciu=format(round(auc_ci[3], 3), nsmall=3), # Upper AUC CI
        statistics=format(round(z, 3), nsmall=3), # Z statistics for AUC
        pvalue=p # z-test p-value
      ))
      
      ## Get the local maximas
      roc_coords <- pROC::coords(roc_obj, x="local maximas", ret="all")
      
      ## Set the note informing the direction of the curve
      if (roc_obj$direction == ">") {
        dirNote <- paste("Predict positive if:",self$options$pred,"> cut-off")
      } else {
        dirNote <- paste("Predict positive if:",self$options$pred,"< cut-off")
      }
      
      ## Fill the cut-offs table
      tableCo <- self$results$tableCo
      tableCo$setTitle(self$options$pred) # title: predictor name
      tableCo$setNote("dir", dirNote, init=FALSE) # note: ROC curve direction
      for (i in seq_along(roc_coords$threshold)) { # one new row per cut-off
        tableCo$addRow(rowKey=i, values=list(
          Cutoff=roc_coords$threshold[i], # cut-off 
          TPR=roc_coords$tpr[i], # sensitivity of the cut-off 
          FPR=roc_coords$fpr[i], # 1-specificity of the cut-off 
          TNR=roc_coords$tnr[i], # specificity of the cut-off 
          FNR=roc_coords$fnr[i], # 1-sensitivity  of the cut-off 
          Accuracy=roc_coords$accuracy[i], # 1-sensitivity  of the cut-off 
          bAccu=mean(roc_coords$tpr[i], roc_coords$tnr[i]), # 
          Precision=roc_coords$ppv[i], #
          NPV=roc_coords$npv[i], #
          Topleft=roc_coords$closest.topleft[i], # top-left dist. of the cut-off 
          Youden=roc_coords$youden[i] # Youden´s J of the cut-off 
        ))          
      }
      
      ##### Plot #####
      
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
        percent=FALSE, # show the sensitivity and specificity as percentages
        legacy.axes=TRUE, # increasing specificity axis (1-Specificity)
        main=self$options$pred, # title: predictor name
        print.thres="local maximas", print.thres.pattern="%.2f", # cut-offs
        print.auc=TRUE, print.auc.x=40, print.auc.y=10, # auc position
        identity.col="red", identity.lty=2 # red dashed discrimination line
      )
      
      ## Notify the rendering system that we have plotted something
      TRUE
      
    } # close the .plot function
    
  ) # close the list
) # and close the R6Class
