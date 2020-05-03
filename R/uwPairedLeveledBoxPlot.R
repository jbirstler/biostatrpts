uwPairedLeveledBoxPlot <- function(pairData,
                                   pairName,
                                   metricName,
                                   factorName,
                                   ptID,
                                   LatexFileName,
                                   pTitle=NULL,
                                   yLab=NULL,
                                   pTest=c("w","t"),
                                   plotMean=FALSE, pOutliers=FALSE,
                                   lWhisker=.05, hWhisker=.95,
                                   lHinge=.25, hHinge=.75,
                                   boxWex=0.75,
                                   printPVals=TRUE,
                                   yLim=NULL,
                                   showTab=FALSE,
                                   printDiff=FALSE, ...)

  {
    pairData[[pairName]] <- as.factor(pairData[[pairName]])
    pairData[[ptID]] <- as.factor(pairData[[ptID]])
    if(length(LatexFileName)!=2)
      {
        warning("LatexFileName needs to be of length 2")
      }
    if(nlevels(pairData[[pairName]]) != 2)
       {
         warning("pairName needs to have two levels only")
       }

    layout(matrix(c(1,1,2,2), nrow=2, byrow=TRUE))  #Makes graphs on top of each other

    uwLeveledBoxPlot(allData=pairData, trxName=pairName,
                     metricName=metricName, lowfactName=factorName,
                     LatexFileName=LatexFileName[1], pTitle=pTitle, yLab=yLab, 
                     printPVals=FALSE, yLim=yLim, Legend=FALSE,...)

    
    # Remove records that have an "NA" in the metric column
    pairData <- pairData[!is.na(pairData[[metricName]]),]

    # Get Difference by each level of factorName
    lFact <- levels(pairData[[factorName]])
    nFact <- length(lFact)
    diffVec <- c()
    diffFact <- c()
    idsVec <- c()
    for(i in 1:nFact)
      {
        thisFact <- subset(pairData, subset=pairData[[factorName]]==lFact[i])
        # Check if more than 1 reading per ptID in ith level of factorName
        if(max(table(thisFact[[pairName]], thisFact[[ptID]])) > 1)
          {stop("There is a ptID with more than one reading in a level of pairName in level of factorName")}
     
        # Find eligible ptID's that have both pre and post
        IDlevels <- levels(thisFact[[ptID]])
        pairVector <- levels(thisFact[[pairName]])
       
        preIDs <- levels(factor(thisFact[[ptID]][thisFact[[pairName]]==pairVector[1]]))
        postIDs <- levels(factor(thisFact[[ptID]][thisFact[[pairName]]==pairVector[2]]))

        checkPre <- IDlevels %in% preIDs
        checkPost <- IDlevels %in% postIDs
        eligibleIDs <- IDlevels[checkPre & checkPost]

        # Narrow data based on eligible ptID's
        pairedData <- subset(thisFact, subset=(thisFact[[ptID]] %in% eligibleIDs))
        pairedData[[ptID]] <- factor(pairedData[[ptID]])

        # Make Sure ordering is correct

        pairedData <- pairedData[order(pairedData[[ptID]], pairedData[[pairName]]),]
        ids <- levels(pairedData[[ptID]])
        idsVec <- c(idsVec, ids)
       
        # Find Differences
        diff <- tapply(pairedData[[metricName]], pairedData[[ptID]], diff)
        diffVec <- c(diffVec, diff)

        # Specify
        fact <- rep(lFact[i], length(diff))
        diffFact <- c(diffFact, fact)
      }

    diffData <- data.frame(diffVec, diffFact, idsVec)
    diffData$diffFact <- ordered(diffData$diffFact, lFact)

    # Create boxplots without plotting
    bp1 <- boxplot(diffData$diffVec~diffData$diffFact, axes=FALSE, plot=FALSE)

    Ns <- tapply(diffData$diffVec, diffData$diffFact, length)

    # alter bp Zekai-style to use the selected quantile intervals
  
    quantile05.values <- tapply(diffData$diffVec, diffData$diffFact,
                                quantile, probs = lWhisker,na.rm=TRUE)
  
    bp1$stats[1,] <- c(quantile05.values[is.na(quantile05.values)==FALSE])
  
    quantile95.values <- tapply(diffData$diffVec, diffData$diffFact,
                                quantile, probs = hWhisker,na.rm=TRUE)
 
    bp1$stats[5,] <- c(quantile95.values[is.na(quantile95.values)==FALSE])
     
    quantile25.values <- tapply(diffData$diffVec, diffData$diffFact,
                                quantile, probs = lHinge, na.rm=TRUE)
   
    bp1$stats[2,] <- c(quantile25.values[is.na(quantile25.values)==FALSE])

    quantile75.values <- tapply(diffData$diffVec, diffData$diffFact,
                                quantile, probs = hHinge, na.rm=TRUE)
    
    bp1$stats[4,] <- c(quantile75.values[is.na(quantile75.values)==FALSE])

    bp1$stats <- round(bp1$stats,2)
    std.values <- round(tapply(diffData$diffVec, diffData$diffFact, sd, na.rm=TRUE),2)
    median.values <- bp1$stats[3,]
    mean.values <- round(tapply(diffData$diffVec,diffData$diffFact, mean, na.rm=TRUE),3)

    min.values <- round(tapply(diffData$diffVec, diffData$diffFact, min, na.rm=TRUE),2)
    max.values <- round(tapply(diffData$diffVec, diffData$diffFact, max, na.rm=TRUE),2)
 
    # reset the matrix structure
    bp1$conf <- matrix(bp1$conf, nrow=2)
    bp1$stats <- matrix(bp1$stats, nrow=5)
    pVal <- c()
    pStr <- c()


    if(printPVals)
      {
        if(substr(pTest[1],1,1) != "w" & substr(pTest[1],1,1) != "t")
          {
            stop("pTest is not a valid choice, must be w or t")
          }
        for(i in 1:nFact)
          {
            if(substr(pTest[1],1,1)=="w")
              {
                pVal[i] <- round(wilcox.test(diffData$diffVec[diffData$diffFact==lFact[i]],
                                             exact=FALSE, )$p.value, 3)
              } 
            else
              {
                pVal[i] <- round(t.test(diffData$diffVec[diffData$diffFact==lFact[i]])$p.value,3)
              }
            if(pVal[i] == 0)
              {
                pVal[i] <- "< 0.001"
                pStr[i] <- paste("pDiff ",pVal[i],sep='')
              }
            else
              {
                pStr[i] <- paste("pDiff = ",pVal[i],sep='')
              }
          }
      }
 
    if (plotMean)
      {
        bp1$stats[3, ] <- mean.values 
        bxp(bp1, xaxt = "n", boxwex=boxWex, ylab=paste("Diff. of", yLab, sep=' '),
            outline = pOutliers, axes=FALSE, main="Paired Differences")

        # Now restore the proper median values
        bp1$stats[3,] <- median.values
      }
    else
      {
        bxp(bp1, xaxt= "n", boxwex=boxWex,
            outline=pOutliers, axes=FALSE,
            medlty="blank", main="Paired Differences", ylab=paste("Diff. of", yLab, sep=' '))
      }

    # add median values in points form
    medLoc <- seq(1:length(bp1$stats[1,]))
    points(x=medLoc, y=median.values, pch=c(16))
    
    # Label bottom axis with pair levels and sample size
    mtext(lFact, side=1, line=1, at=c(1:nFact), cex=0.7)
    mtext(Ns, side=1, line=0, at=c(1:nFact), cex=0.6)
    mtext("Pairs", side=1, line=0, at=par("usr")[1], adj=1, cex=0.6)

    # Place horizontal dotted line at 0

    abline(h=0, lty=2)

    # Label top with p-values
    if(printPVals)
      {
        mtext(pStr, side=3, at=c(1:nFact), cex=0.7)
      }
    
    #axes
    axis(2, at=axTicks(2), labels=paste(axTicks(2),sep=''),
         las=2, cex.axis=0.8)
    
    box()

    table.values<-cbind("Factor" = lFact, "Pairs"=Ns,
                        "Mean"=format(mean.values,digits=2,nsmall=2),
                        "SD"=format(std.values,digits=2,nsmall=2),
                        "Min"=format(min.values,digits=2,nsmall=2),
                        "P5"=format(bp1$stats[1,],digits=2,nsmall=2),
                        "Q1"=format(bp1$stats[2,],digits=2,nsmall=2),
                        "Median"=format(bp1$stats[3,],digits=2,nsmall=2),
                        "Q3"=format(bp1$stats[4,],digits=2,nsmall=2),
                        "P95"=format(bp1$stats[5,],digits=2,nsmall=2),
                        "Max"=format(max.values,digits=2,nsmall=2))
    colnames(table.values)[1] <- ""

    if(printPVals)
      {
        table.values <- cbind(table.values, "Contrast"=rep("Diff=0", nFact), "P-Value"=pVal)
      }

    rownames(table.values) <- NULL
    
    if(printPVals)
      {
        col.just <- c("|l|",rep("c", 10),"|r","r|")
      }
    else
      {
        col.just <- c("|l|",rep("c", 9), "c|")
      }
    ltxTbl <- uwLatex(mat=table.values, file=LatexFileName[2],
                      caption=paste("Diff. of", yLab, sep=' '),
                      col.just=col.just, rowLines=FALSE, size="small")
      
    if(showTab)
      {structure(list(file=LatexFileName[2]), class='latex')}

    if(printDiff)
      {
        return(diffData)
      }
  }
