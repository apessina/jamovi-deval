
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
      
      ## Option values into shorter variable names
      pred  <- self$options$pred
      resp <- self$options$resp
      
      ## Check if variables have any data
      if (is.null(pred) || is.null(resp))
        return()
      
      ## Get the data
      data <- self$data
      
      ## Convert to appropriate data types
      data[[pred]] <- jmvcore::toNumeric(data[[pred]])
      data[[resp]] <- jmvcore::toNumeric(data[[resp]])
      
      ## Remove NAs
      data <- na.omit(data)

      ## Define data variables
      predictor <- data[[pred]]
      response <- data[[resp]]
      
      ## Check if there is more than one unique value in the predictor
      if (length(unique(predictor)) <= 1) {
        stop("Predictor has no variability.")
      }
      
      ## Check if the outcome variable have only 0 and 1 values
      if (!all(response %in% c(0, 1))) {
        stop("Inform only 1 for Cases and 0 for Controls (healthy) in the response variable.")
      }
      
      ## Check if the outcome variable have both 0 and 1 values
      if (length(unique(response)) != 2) {
        stop("The response variable must contain both 0 and 1.")
      }
      
      ##### Calculations #####
      
      if (self$options$direction=="greater") {
        direction <- ">"
      } else if (self$options$direction=="less") {
        direction <- "<"
      } else {
        direction <- "auto"
      }
      
      ##  Calculate the ROC Curve using pROC library
      roc_obj <- pROC::roc(response, predictor, 
                           levels=c(0, 1), direction=direction)
      
      ## Calculate the Area Under the Curve with confidence interval
      auc_ci <- pROC::ci.auc(roc_obj, conf.level=self$options$ciWidth/100, 
                             method="delong") # CI with DeLong's variance
      
      ## Calculate DeLong's Z with H₁: AUC > 0.5
      auc_se <- sqrt(pROC::var(roc_obj, method="delong")) # standard error
      z <- (pROC::auc(roc_obj) - 0.5) / auc_se # Z statistics
      p <- 1 - pnorm(z) # unilateral p-value
      
      ## Calculate means and medians for groups
      groups_mean <- aggregate(predictor, by=list(group=response), FUN=mean)
      groups_median <- aggregate(predictor, by=list(group=response), FUN=median)
      control_mean <- groups_mean$x[groups_mean$group==0]
      case_mean <- groups_mean$x[groups_mean$group==1]
      control_median <- groups_median$x[groups_median$group==0]
      case_median <- groups_median$x[groups_median$group==1]
      ### Make direction suggestion
      if (control_median > case_median) {
        suggest <- "Control > Case"
      } else if (control_median < case_median) {
        suggest <- "Control < Case"
      } else {
        suggest <- NA
      }
      
      ##### Results ####
      
      ## Fill the main results table
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
      
      if (self$options$boolz)
        tableMain$setNote("hip", "H₁: AUC > 0.5", init=FALSE)
      
      ## Fill the groups table
      tableDir <- self$results$tableDir
      tableDir$setRow(rowNo=1, values=list(
        control_median=format(round(control_median, 3), nsmall=3), 
        case_median=format(round(case_median, 3), nsmall=3),
        control_mean=format(round(control_mean, 3), nsmall=3), 
        case_mean=format(round(case_mean, 3), nsmall=3),
        suggest=suggest
      ))
      
      ## Get the local maximas
      roc_coords <- pROC::coords(roc_obj, x="local maximas", ret="all")
      
      ## Order lines
      roc_coords <- roc_coords[order(roc_coords[[self$options$sortBy]], 
                                     decreasing=self$options$decrease), ]
      
      ## Set the note informing the direction of the curve
      if (roc_obj$direction == ">") {
        dirNote <- paste("Positive if", self$options$pred, "≤ cut-off")
      } else if (roc_obj$direction == "<") {
        dirNote <- paste("Positive if", self$options$pred, "≥ cut-off")
      } else {
        stop(
          paste0("Invalid direction when calculating cut-offs.\n",
                 "Check if the input data can generate a valid ROC curve.")
        )
      }
      
      ## Set lines limit
      lm_lines <- self$options$coLines
      if (lm_lines > nrow(roc_coords))
        lm_lines <- nrow(roc_coords)
      
      ## Fill the cut-offs table
      tableCo <- self$results$tableCo
      for (i in seq_len(lm_lines)) { # one new row per cut-off
        tableCo$addRow(rowKey=i, values=list(
          var = if (i==1) self$options$pred,
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
      
      tableCo$setNote("dir", dirNote, init=FALSE) # note: ROC curve direction
      
      ##### Plot #####
      
      ## Set-up the plot object with the plot data in a dataframe
      image <- self$results$plot
      image$setState(
        list(
          df=data.frame(response, predictor),
          direction=direction
        )
      )
      
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
        image$state$df$response, image$state$df$predictor, # variables
        levels=c(0, 1), direction=image$state$direction, # direction of the curve
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
