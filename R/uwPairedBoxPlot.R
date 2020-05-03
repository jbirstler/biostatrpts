############################
# Function uwPairedBoxPlot 
############################



#' Box Plots of Paired Numeric Variables
#' 
#' This function is meant for the creation of box plots for paired numeric
#' variables.  This function also produces the LaTeX code needed to create the
#' corresponding LaTeX table summarizing the box plots numerically.
#' 
#' Additional Packages Required: Hmisc for latex function to create table.
#' 
#' Function matches ptID readings for pre (first level of pairName) to
#' corresponding post (second level of pairName) reading.  A Wilcoxon Signed
#' Rank Test for paired data is conducted on the matched pairs and that p-value
#' is reported. The difference box is shown as post-pre.
#' 
#' @param allData Data frame with relevant variables.
#' @param pairName String of the factor variable name in allData that gives the
#' two levels being compared.
#' @param metricName String of the variable name that represents the numeric
#' variable in allData
#' @param ptID String of the variable name that represents the patient
#' identification variable in allData
#' @param onlyPairs Logical.  TRUE implies only the ptID numbers with both pre-
#' and post-scores in metricName will be used in the individual plots.  FALSE
#' implies all data will be included in the individual box plots.
#' @param plotDiff Logical. TRUE implies that the box plot of the difference
#' (of full pairs only) will be plotted also. See chgLayout and layout. FALSE
#' implies only the individual plots will be plotted.
#' @param plotDiffOnly Logical. TRUE only the paired difference boxplot will be
#' plotted.  LaTeX table will still have all data in it.
#' @param pctChange Logical. TRUE will give the percent change from baseline
#' instead of differences. plotDiff must be TRUE to work
#' @param chgLayout Logical. TRUE implies the layout of the plotting screen
#' should be changed inside the function.  The layout must be changed if
#' plotDiff=T.  This can be done in one of two ways: manually or with this
#' argument.  Manually must be done before this function is run.  This is done
#' with the layout() function. This is beneficial if you want to put more than
#' the two plots produced by this function on one plot.  Alternatively the
#' layout can be changed in the function by chgLayout==T the default is set to
#' layout(matrix(c(1,1,2),nrow=1))
#' @param pWilcox Logical: TRUE implies Wilcoxon Signed Rank p-value on the
#' paired differences should be given.  FALSE gives paired T-test p-value.
#' @param LatexFileName String. Giving the folder and file(ending in .tex) for
#' the LaTeX table to be saved. The default is NULL, which will create a table
#' in the current directory of the R session, and will have the file name of
#' the metricName.tex.
#' @param pTitle String of the Title of the plot of the individual box plots.
#' @param yLab String of the y-axix label of the plot of the individual box
#' plots.
#' @param plotMean Logical. TRUE -> Gives a line in the box plot that shows
#' where the mean is.
#' @param pOutliers Logical.  TRUE -> Plots outliers
#' @param lWhisker,hWhisker Percentages to depict the whiskers of the box plot.
#' l for lower whisker and h for higher whisker.
#' @param lHinge,hHinge Percentages where the corners of the boxes be. Default
#' is 1st and 3rd quartiles
#' @param boxWex Numeric between 0 and 1 for width of boxes.
#' @param printPVals Logical. Do you want P-values to be reported?
#' @param yLim Limits of the y-axis. In format: c(start, finish)
#' @param footNote String. Placed as a footnote in the bottom left side of the
#' plot margin.
#' @param firstCol.name (string) Name for first column, which is like a title
#' for the rows.
#' @param LatexCaption String to be used as the caption for the LaTeX table.
#' Defaults to metricName.
#' @param showTab Logical. TRUE prints LaTeX table using structure() function
#' @param ... Any argument that can be passed to uwLatex()
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S. and Frontier Science and
#' Technology Research Foundation, Patrick Lenon and Zekai Otles
#' @examples
#' 
#' 
#' Time <- ordered(c(rep("Pre", 100), rep("Post", 100)), c("Pre","Post"))
#' ID <- rep(sample(c(1:100),100,replace=FALSE),2) #IDs will not be in order
#' weight <- c(rnorm(100,200,3), rnorm(100,197,3)) #pretend they are paired
#' 
#' ID[3] <- NA
#' ID[176] <- NA   #Possible missing data
#' ID[150] <- 101  #Say someone as only one reading in post
#' 
#' dat <- data.frame(Time, ID, weight)
#' 
#' # onlyPairs=T, plotDiff=T
#' uwPairedBoxPlot(allData=dat,
#'                 pairName="Time",
#'                 metricName="weight",
#'                 ptID="ID",
#'                 onlyPairs=TRUE,
#'                 plotDiff=TRUE,
#'                 chgLayout=TRUE,
#'                 pWilcox=TRUE,
#'                 LatexFileName=NULL,
#'                 pTitle="uwPairedBoxPlot Example",
#'                 yLab="Weight (lbs.)",
#'                 plotMean=FALSE, pOutliers=FALSE,
#'                 lWhisker=.05, hWhisker=.95,
#'                 lHinge=.25, hHinge=.75,
#'                 boxWex=.75,
#'                 printPVals=TRUE,
#'                 yLim=NULL,
#'                 footNote=NULL,
#'                 firstCol.name=NULL,
#'                 LatexCaption=NULL,
#'                 caption.loc="bottom",
#'                 showTab=FALSE,
#'                 label=NULL)
#' 
#' # onlyPairs=F, plotDiff=T
#' uwPairedBoxPlot(allData=dat,
#'                 pairName="Time",
#'                 metricName="weight",
#'                 ptID="ID",
#'                 onlyPairs=FALSE,
#'                 plotDiff=TRUE,
#'                 chgLayout=TRUE,
#'                 pWilcox=TRUE,
#'                 LatexFileName=NULL,
#'                 pTitle="uwPairedBoxPlot Example",
#'                 yLab="Weight (lbs.)",
#'                 plotMean=FALSE, pOutliers=FALSE,
#'                 lWhisker=.05, hWhisker=.95,
#'                 lHinge=.25, hHinge=.75,
#'                 boxWex=.75,
#'                 printPVals=TRUE,
#'                 yLim=NULL,
#'                 footNote=NULL,
#'                 firstCol.name=NULL,
#'                 LatexCaption=NULL,
#'                 caption.loc="bottom",
#'                 showTab=FALSE,
#'                 label=NULL)
#' 
uwPairedBoxPlot <- function(allData,
                            pairName,
                            metricName,
                            ptID,
                            onlyPairs=TRUE,
                            plotDiff=TRUE,
                            plotDiffOnly=FALSE,
                            pctChange=FALSE,
                            chgLayout=TRUE,
                            pWilcox=TRUE,
                            LatexFileName=NULL,
                            pTitle=NULL,
                            yLab=NULL,
                            plotMean=FALSE, pOutliers=FALSE,
                            lWhisker=.05, hWhisker=.95,
                            lHinge=.25, hHinge=.75,
                            boxWex=.75,
                            printPVals=TRUE,
                            yLim=NULL,
                            footNote=NULL,
                            firstCol.name=NULL,
                            LatexCaption=NULL,
                            showTab=FALSE, ...)
  {

    if(!plotDiff & plotDiffOnly)
      {
        print("plotDiff changed to TRUE since plotDiffOnly is TRUE")
        plotDiff <- TRUE
      }
    # P-value not necessary if plotDiff=F and onlyPairs=F
    if(!plotDiff & !onlyPairs)
      {
        print("printPVals set to FALSE with !plotDiff & !onlyPairs")
        printPVals <- FALSE
      }
    if(is.null(LatexCaption))
      {
        LatexCaption <- metricName
      }
    # Remove records that have an "NA" in the metric column
    allData <- allData[!is.na(allData[[metricName]]),]

    # strip the input dataset down to only the relevant columns
    allData <- subset(allData,
                    select = c(pairName, ptID, metricName))

    # re-factor
    allData[[pairName]] <- factor(allData[[pairName]])
    allData[[ptID]] <- factor(allData[[ptID]])

    # Check if more than 1 reading per ptID in either level of pairName
    occurTab <- table(allData[[pairName]],allData[[ptID]])
    if(sum(occurTab[1,]>1)>0)
      {stop("There is a ptID with more than one reading in the first level of pairName")}
    if(sum(occurTab[2,]>1)>0)
      {stop("There is a ptID with more than one reading in the second level of pairName")}

    # full N's
    fullN <- table(allData[[pairName]])

    # Find eligible ptID's that have both pre and post 
    IDlevels <- levels(allData[[ptID]])
    pairVector <- levels(allData[[pairName]])

    preIDs <- levels(factor(allData[[ptID]][allData[[pairName]]==pairVector[1]]))
    postIDs <- levels(factor(allData[[ptID]][allData[[pairName]]==pairVector[2]]))

    checkPre <- IDlevels %in% preIDs
    checkPost <- IDlevels %in% postIDs
    eligibleIDs <- IDlevels[checkPre & checkPost]

    # Narrow data based on eligible ptID's

    pairedData <- subset(allData,
                      subset=(allData[[ptID]] %in% eligibleIDs))

    # Order data frame so ptID's are matching for Pre and Post

    pairedData <- pairedData[order(pairedData[[pairName]],pairedData[[ptID]]),]
    
    # re-factor
    pairedData[[pairName]] <- factor(pairedData[[pairName]])
    pairedData[[ptID]] <- factor(pairedData[[ptID]])

    # Get Difference vector
    totN <- length(pairedData[[metricName]])
    diffVec <- pairedData[[metricName]][((totN/2)+1):totN]-
      pairedData[[metricName]][1:(totN/2)]

    if(pctChange)
        diffVec <- 100*(diffVec/(pairedData[[metricName]][1:(totN/2)]))
      
    # run boxplots without plotting to generate boxplot stats.
    if(onlyPairs)
      {
        bp1 <- boxplot(pairedData[[metricName]] ~ pairedData[[pairName]],
                axes=FALSE, plot=FALSE)
        Nstr <- rep(paste("n = ",totN/2,sep=''), 2)
        tableN <- rep(totN/2,2)
        allData <- pairedData # so only one set of code the rest of the way
      }
    else
      {
        bp1 <- boxplot(allData[[metricName]] ~ allData[[pairName]],
                       axes=FALSE, plot=FALSE)
        Nstr <- c(paste("n = ",fullN[1],sep=''), paste("n = ", fullN[2],sep=''))
        tableN <- fullN
      }
    if(plotDiff)
      {
        bp2 <- boxplot(diffVec, axes=FALSE, plot=FALSE)
        bp2$stats[1] <- round(quantile(diffVec, probs=lWhisker, na.rm=TRUE),2)
        bp2$stats[5] <- round(quantile(diffVec, probs=hWhisker, na.rm=TRUE),2)
        bp2$stats[2] <- round(quantile(diffVec, probs=lHinge, na.rm=TRUE),2)
        bp2$stats[4] <- round(quantile(diffVec, probs=hHinge, na.rm=TRUE),2)
        std.values2 <- round(sd(diffVec, na.rm=TRUE),2)
        median.values2 <- round(bp2$stats[3],2)
        mean.values2 <- round(mean(diffVec, na.rm=TRUE),2)
        min.values2 <- round(min(diffVec, na.rm=TRUE),2)
        max.values2 <- round(max(diffVec, na.rm=TRUE),2)
      }
    # alter bp Zekai-style to use the selected quantile intervals
    
    quantile05.values <- tapply(allData[[metricName]],
                                list(allData[[pairName]]),
                                quantile, probs = c(lWhisker),
                                na.rm=TRUE)
    
    bp1$stats[1,] <- c(quantile05.values[is.na(quantile05.values)==FALSE])
    
    quantile95.values <- tapply(allData[[metricName]],
                                list(allData[[pairName]]),
                                quantile, probs = c(hWhisker),
                                na.rm=TRUE)
    
    bp1$stats[5,] <- c(quantile95.values[is.na(quantile95.values)==FALSE])
    
    quantile25.values <- tapply(allData[[metricName]],
                                list(allData[[pairName]]),
                                quantile, probs = c(lHinge),
                                na.rm=TRUE)
    
    bp1$stats[2,] <- c(quantile25.values[is.na(quantile25.values)==FALSE])
    
    quantile75.values <- tapply(allData[[metricName]],
                                list(allData[[pairName]]),
                                quantile, probs = c(hHinge),
                                na.rm=TRUE)
    
    bp1$stats[4,] <- c(quantile75.values[is.na(quantile75.values)==FALSE])
    
    bp1$stats <- round(bp1$stats,2)
    std.values1 <- round(tapply(allData[[metricName]],
                                list(allData[[pairName]]),
                                sd, na.rm=TRUE),2)
    
    min.values1 <- round(tapply(allData[[metricName]], list(allData[[pairName]]),
                                min, na.rm=TRUE),2)
    
    max.values1 <- round(tapply(allData[[metricName]], list(allData[[pairName]]),
                                max, na.rm=TRUE),2)
    
  # reset the matrix structure
    bp1$conf <- matrix(bp1$conf, nrow=2)
    bp1$stats <- matrix(bp1$stats, nrow=5)

    if(printPVals)
      {
        if(pWilcox)
          {
            pVal <- round(wilcox.test(pairedData[[metricName]]~pairedData[[pairName]],
                                      paired=TRUE, exact=FALSE)$p.value,3)
          }
        else
          {
            pVal <- round(t.test(pairedData[[metricName]]~pairedData[[pairName]],
                                 paired=TRUE, exact=FALSE)$p.value,3)
          }
        if(pVal == 0)
          {pVal <- "< 0.001"
           pStr <- paste("pDiff ",pVal,sep='')
         }
        else
          {
            pStr <- paste("pDiff = ",pVal,sep='')
          }
      }
    
    median.values1 <- bp1$stats[3,]
    mean.values1 <- round(tapply(allData[[metricName]],
                                 allData[[pairName]],
                                 mean, na.rm=TRUE),2)
    
    # Change Layout so graphs will fit next to each other

    if(chgLayout)
      {
        if(plotDiff & !plotDiffOnly)
          {
            lty <- layout(matrix(c(1,1,2), nrow=1))
          }
        else
          {
            lty <- layout(1)
          }
      }
 
    if(!plotDiffOnly)
      {
        if (plotMean)
          {
            bp1$stats[3, ] <- mean.values1 
            bxp(bp1, ylab = yLab,xaxt = "n", boxwex=boxWex,
                outline = pOutliers, axes=FALSE, ylim=yLim)

        # Now restore the proper median values
            bp1$stats[3,] <- median.values1
          }
        else
          {
            bxp(bp1, ylab=yLab, xaxt= "n", boxwex=boxWex,
                outline=pOutliers, axes=FALSE, ylim=yLim,
                medlty="blank")
          }
    # add median values in points form

        medLoc <- seq(1:length(bp1$stats[1,]))
        points(x=medLoc, y=median.values1, pch=c(16))

    # Label bottom axis with pair levels and sample size

        mtext(pairVector, side=1, line=1, at=c(1:2), cex=0.7)
        mtext(Nstr, side=1, line=0, at=c(1:2), cex=0.6)

        if(!plotDiff & printPVals)
          {
            mtext(pStr, side=3, cex=0.7)
          }
    #title
        title(main=pTitle)

    #axes
        axis(2, at=axTicks(2), labels=paste(axTicks(2),sep=''),
             las=2, cex.axis=0.8)

    #footnote
        if(is.character(footNote))
          {
            mtext(footNote, side=1, line=3, at=.5, cex=0.6)
          }

        box()
      }

    if(plotDiff)
      {
        if(pctChange)
          yLab <- "Percent Change"
        else
          yLab <- "Difference"
        
        if (plotMean)
          {
            bp2$stats[3] <- mean.values2 
            bxp(bp2, ylab = yLab, xaxt = "n", boxwex=boxWex,
                outline = pOutliers, axes=FALSE, ylim=NULL)
            
        # Now restore the proper median values
            bp2$stats[3] <- median.values2
          }
        else
          {
            bxp(bp2, ylab= yLab, xaxt= "n", boxwex=boxWex,
                outline=pOutliers, axes=FALSE, ylim=NULL,
                medlty="blank")
          }
    # print p-values at top of page
        if(printPVals)
          {
            mtext(pStr, side=3, cex=0.7)
          }
    # add median values in points form

        points(x=1, y=median.values2, pch=c(15))

    # Label bottom axis with pair levels and sample size

        mtext(paste("Pairs = ", totN/2,sep=''), side=1, line=0, cex=0.6)
      
    #title
        if(!plotDiffOnly)
          {
            if(pctChange)
              title(main="Paired Percent Change from Baseline")
            else
              title(main="Paired Difference")
          }
        else
          {
            if(is.null(pTitle))
              {
                if(pctChange)
                  title(main="Paired Percent Change from Baseline")
                else
                  title(main="Paired Difference")
              }
            else
              {
                title(main=pTitle)
              }
          }
    #axes
        axis(2, at=axTicks(2), labels=paste(axTicks(2),sep=''),
             las=2, cex.axis=0.8)

        box()
      }
    # Make LaTeX table

    LatexTab <- cbind("Pair"=pairVector, "N"=tableN, "Mean"=mean.values1,
                      "SD"=std.values1, "Min"=min.values1, "P5"=bp1$stats[1,],
                      "Q1"=bp1$stats[2,],"Median"=median.values1, "Q3"=bp1$stats[4,],
                      "P95"=bp1$stats[5,], "Max"=max.values1)
    col.just <- c("l",rep("r",10))
    if(plotDiff)
      {
        if(pctChange)
          diffName <- "Pct Change"
        else
          diffName <- "Diff."
        
        diffSummary <- c(diffName,totN/2, mean.values2, std.values2, min.values2, bp2$stats[1,],
                         bp2$stats[2,], median.values2, bp2$stats[4,],
                         bp2$stats[5,], max.values2)
        LatexTab <- rbind(LatexTab, diffSummary)
        
        if(printPVals)
          {
            pvCol <- c("","",pStr)
            LatexTab <- cbind(LatexTab, "P-Value"=pvCol)
            col.just <- c("l",rep("r",11))
          }
      }
    else
      {
        if(printPVals)
          {
            pvCol <- c("",pStr)
            LatexTab <- cbind(LatexTab, "P-Value"=pvCol)
            col.just <- c("l",rep("r",11))
          }
      }
    rownames(LatexTab) <- NULL

    if(is.character(firstCol.name))
      {
        colnames(LatexTab)[1] <- firstCol.name
      }
    else
      {
        colnames(LatexTab)[1] <- metricName
      }
    if(is.null(LatexFileName))
      {
        print(LatexTab)
        showTab <- FALSE
        warning("LatexFileName is NULL. Table not saved.")
      }
    else
      {
        ltxTbl <- uwLatex(mat=LatexTab, file=LatexFileName, caption=LatexCaption, ...)
      }
    if(showTab)
      {
        structure(list(file=LatexFileName), class='latex')
      }
  }
