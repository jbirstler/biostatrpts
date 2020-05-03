## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## The text of the GNU General Public License, version 2, is available
## as http://www.gnu.org/copyleft or by writing to the Free Software
## Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
############################
# Function uwLeveledBoxPlot#
############################



#' Box plots with Two factor variables
#' 
#' This function will produce a graphic and statistical testing for a numeric
#' variable when stratified by two factor variables.  Generally one of the
#' factor variables is considered the treatment variable.  Also produced is
#' code for a corresponding LaTeX table.
#' 
#' Special Functions used in uwLeveledBoxPlot: uwPVals and uwLatex
#' 
#' Plotting points in the boxes represent the median value.  A line would
#' represent the mean if plotMean=TRUE.
#' 
#' @param allData Data frame in R that contains all of the variables
#' @param trxName String. Name of the treatment variable in allData
#' @param metricName String. Name of the numeric variable in allData that will
#' be represented by boxes
#' @param lowfactName String. Name of the low-level factor variable in allData
#' that will be used to stratify. Make the factor an ordered factor if order
#' matters in the reporting.
#' @param hifactName String. Name of the upper-level factor variable in allData
#' that will be used to stratify first, and then stratify on lowfactName.
#' @param delta Logical.  TRUE looks at differences from baseLevel and other
#' levels of hifactName or lowfactName is no hifactName is given
#' @param deltaPct Logical. TRUE gives percentage change from baseLevel. delta
#' must also be TRUE
#' @param baseLevel String. Level in hifactName or lowfactName if no hifactName
#' is given that is considered the baseline level
#' @param idName String.  Variable in allData that gives patient identifiers.
#' Only necessary if delta=TRUE
#' @param LatexFileName String.  Giving the file path and name, ending in .tex,
#' where the LaTeX code for the table should be saved.  If NULL, table is
#' printed in the R session but not saved.
#' @param LatexCaption uwLatex, Caption for Latex table
#' @param lowfacttableName String to be given to the lowfactName in the LaTeX
#' table
#' @param hifacttableName String to be given to the hifactName in the LaTeX
#' table
#' @param showTab Logical. TRUE -> Displays the compiled LaTeX table using dvix
#' @param pTitle String. Title of the Plot and caption of the LaTeX table
#' @param yLab String. Label for the y-axis.
#' @param yLim Limits of y-axix. In the form: c(start,finish)
#' @param plotMean Logical. TRUE -> A line is plotted in the box plots showing
#' the mean value.  A point is always plotted showing the median value.
#' @param pOutliers Logical. TRUE -> Show points outside of whiskers
#' @param lWhisker Lower whisker level. Default is .05 for 5th percentile
#' @param hWhisker Upper whisker level. Default is .95 for 95th percentile
#' @param lHinge Lower level of box. Default is .25 for 25th percentile
#' @param hHinge Upper level of box. Default is .75 for 75th percentile
#' @param boxWex Number for width of boxes
#' @param LabelCex Numeric.  Gives magnification level of the labels in the
#' x-axis. Less than 1 shrinks them, greater than 1 expands them.
#' @param Legend Logical.  TRUE puts a legend of the trxName levels in the plot
#' and removes the labeling in the x-axis for trxName levels
#' @param LegendLoc Location of the legend if Legend=TRUE, see help(legend)
#' @param LegendCex Numeric.  Gives magnification level of the legend
#' @param printPVals Logical. TRUE -> Prints p-values on the top of the graph
#' and adds them to the LaTeX table
#' @param PVinTableOnly Logical. TRUE Suppresses the printing of p-values in
#' the top margin of the graph, but they are still printed in the LaTeX table.
#' Useful when p-values are hard to read in the margin
#' @param PValCex Numeric.  Gives magnification level of the p-values in the
#' top margin
#' @param pTest printPVals must be TRUE. Chose type of comparison test
#' @param abbrevN Whole number indicating how many letters should abbreviation
#' of the treatments levels should be.
#' @param pairwise (logical) TRUE pairwise comparisons should be made between
#' treatment levels.  FALSE multi-level test occurs between all levels of the
#' treatment
#' @param pAdjust NULL for none, "h" for "holm", or "b" for "bonferroni".
#' P-value adjustment methods.  See uwPVals for more information.
#' @param trxControl Must be defined if pairwise=TRUE. A treatment level is
#' indicated here as the control level, two-way comparisons with the control
#' and non-controls will then be made.
#' @param pExclude only applies if pairwise=TRUE. This should be a vector of
#' non-control treatment levels that will be looked at one at a time and that
#' non-control's contrast will not be reported
#' @param pInclude List specifying contrasts want to be examined. This is most
#' useful when wanting to combine treatment levels together.  See uwPVals for
#' more details
#' @param Ropen Logical.  TRUE collapses data across all treatment arms.
#' @param numDec Numeric from 0 to 20 indicating number of decimals to be
#' reported in the tables
#' @param ... Any other argument that can be passed to uwLatex other than mat,
#' file, or col.just which are already set
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S. and Frontier Science and
#' Technology Research Foundation, Patrick Lenon
#' @seealso uwBoxPlot(), uwLatex()
#' @examples
#' 
#' 
#' trt1 <- rep(rep(c("A","B","C"), 120),3)
#' fctr1 <- as.factor(rep(c(rep("Female",180),rep("Male",180)),3))
#' mtrc1 <- rnorm(360*3, 10, 4)
#' upfct <- c(rep("Baseline",360),rep("Week 12",360), rep("Week 24",360))
#' ids <- as.character(c(1:360,1:360,1:360))
#' dat1 <- data.frame(trt1,fctr1,mtrc1,upfct,ids)
#' 
#' lyt <- layout(matrix(c(1,1,2,2,3,3), nrow=3, byrow=TRUE))
#' 
#' uwLeveledBoxPlot(allData=dat1, 
#' 		trxName="trt1", 
#' 		metricName="mtrc1", 
#' 		lowfactName="fctr1",
#'                 hifactName="upfct",
#'                 delta=FALSE,
#'                 baseLevel="Baseline",
#'                 idName="ids",
#' 		LatexFileName=NULL,
#' 		showTab=FALSE,
#'                 Legend=FALSE,
#' 		pTitle="Example of uwLeveledBoxPlot",
#' 		yLab="Random Normal", 
#' 		plotMean=TRUE, pOutliers=FALSE,
#'                 lWhisker=.05, hWhisker=.95,
#' 		lHinge=.25, hHinge=.75,
#' 		boxWex=0.75,
#' 		printPVals=TRUE,
#'                 PVinTableOnly=FALSE,
#'                 PValCex=0.5,
#'                 Ropen=FALSE,
#'                 pTest="t",
#'                 abbrevN=1,
#'                 pairwise=TRUE,
#'                 trxControl=NULL,
#' 		pExclude=NULL,
#' 		yLim=NULL)
#' 
#' uwLeveledBoxPlot(allData=dat1, 
#' 		trxName="trt1", 
#' 		metricName="mtrc1", 
#' 		lowfactName="fctr1",
#'                 hifactName="upfct",
#'                 delta=TRUE,
#'                 deltaPct=FALSE,
#'                 baseLevel="Baseline",
#'                 idName="ids",
#' 		LatexFileName=NULL,
#' 		showTab=FALSE,
#'                 Legend=TRUE,
#'                 LegendLoc="topleft",
#' 		pTitle="Example of uwLeveledBoxPlot with delta=TRUE",
#' 		yLab="Random Normal", 
#' 		plotMean=FALSE, pOutliers=FALSE,
#'                 lWhisker=.05, hWhisker=.95,
#' 		lHinge=.25, hHinge=.75,
#' 		boxWex=0.75,
#' 		printPVals=TRUE,
#'                 PVinTableOnly=FALSE,
#'                 PValCex=0.5,
#'                 Ropen=FALSE,
#'                 pTest="w",
#'                 abbrevN=1,
#'                 pairwise=TRUE,
#'                 trxControl=NULL,
#' 		pExclude=NULL,
#'                 pInclude=list(list(c("A","B"),"C"),list("A",c("B","C")),list("A","C")),
#' 		yLim=NULL)
#' 
#' uwLeveledBoxPlot(allData=dat1, 
#' 		trxName="trt1", 
#' 		metricName="mtrc1", 
#' 		lowfactName="fctr1",
#'                 hifactName="upfct",
#'                 delta=TRUE,
#'                 deltaPct=TRUE,
#'                 baseLevel="Baseline",
#'                 idName="ids",
#' 		LatexFileName=NULL,
#' 		showTab=FALSE,
#'                 Legend=TRUE,
#'                 LegendLoc="topleft",
#' 		pTitle="Example of uwLeveledBoxPlot with deltaPct=TRUE",
#' 		yLab="Random Normal", 
#' 		plotMean=FALSE, pOutliers=FALSE,
#'                 lWhisker=.05, hWhisker=.95,
#' 		lHinge=.25, hHinge=.75,
#' 		boxWex=0.75,
#' 		printPVals=TRUE,
#'                 PVinTableOnly=FALSE,
#'                 PValCex=0.5,
#'                 Ropen=FALSE,
#'                 pTest="w",
#'                 abbrevN=1,
#'                 pairwise=TRUE,
#'                 trxControl=NULL,
#' 		pExclude=NULL,
#'                 pInclude=list(list(c("A","B"),"C"),list("A",c("B","C")),list("A","C")),
#' 		yLim=NULL)
#' 
uwLeveledBoxPlot <- function(allData,
                             trxName,
                             metricName,
                             lowfactName,
                             hifactName=NULL,
                             delta=FALSE,
                             deltaPct=FALSE,
                             baseLevel=NULL,
                             idName=NULL,
                             LatexFileName=NULL,
                             LatexCaption=NULL,
                             lowfacttableName="",
                             hifacttableName="",
                             showTab=FALSE,
                             pTitle=NULL,
                             yLab=NULL,
                             yLim=NULL,
                             plotMean=FALSE, pOutliers=FALSE,
                             lWhisker=.05, hWhisker=.95,
                             lHinge=.25, hHinge=.75,
                             boxWex=0.75,
                             LabelCex=0.7,
                             Legend=FALSE,
                             LegendLoc="topright",
                             LegendCex=0.7,
                             printPVals=TRUE,
                             PVinTableOnly=FALSE,
                             PValCex=0.6,
                             pTest=c("wilcox","t.test"),
                             abbrevN=1,
                             pairwise=TRUE,
                             pAdjust=NULL,
                             trxControl=NULL,
                             pExclude=NULL,
                             pInclude=list(list(NULL,NULL)),
                             Ropen=FALSE,
                             numDec=2,
                             ...)
{
  if(Ropen)
    {
      printPVals <- FALSE
      Legend <- FALSE
    }
  
  library(gdata)
# shape palette for use in drawing points
  trxPch <- c(19, 21, 15, 0, 17, 2, 48:57)

# Remove records that have an "NA" in the metric column
 
  allData <- allData[!is.na(allData[[metricName]]),]

 # Make sure trxName is a factor
  if(!is.factor(allData[[trxName]]))
    allData[[trxName]] <- as.factor(allData[[trxName]])
  
  allData[[trxName]] <- drop.levels(allData[[trxName]],reorder=FALSE)
  allData[[lowfactName]] <- drop.levels(allData[[lowfactName]], reorder=FALSE)
 
  trxVector <- levels(allData[[trxName]])
  lowfactVec <- levels(allData[[lowfactName]])
  trxN <- length(trxVector)
  lowfactN <- length(lowfactVec)
  if(!is.null(hifactName))
    {
      allData[[hifactName]] <- drop.levels(allData[[hifactName]],reorder=FALSE)
      hifactVec <- levels(allData[[hifactName]])
      hifactN <- nlevels(allData[[hifactName]])
    }
  else{hifactN <- NULL}

  # Get differences if baseline delta=TRUE
  if(delta)
    {
      if(!is.null(hifactName))
        {
          diffData <- data.frame()
          for(i in 1:length(lowfactVec))
            {
              thisLowFact <- subset(allData, allData[[lowfactName]]==lowfactVec[i])
              if(!(baseLevel %in% levels(thisLowFact[[hifactName]])))
                {
                  stop("baseLevel is not a level in hifactName")
                }
              diffDat <- diffFunction(Diffdata=thisLowFact, variable=metricName,
                                      TrxName=trxName, Baseline=baseLevel,
                                      VisitName=hifactName,ptID=idName,deltaPct=deltaPct)
              
              diffDat$lowFact <- rep(lowfactVec[i],length(diffDat$diffCol))
              diffData <- rbind(diffData, diffDat)
            }
          diffData$lowFact <- factor(diffData$lowFact, lowfactVec, ordered=TRUE)
          allData <- as.data.frame(diffData)
          colnames(allData) <- c(idName,trxName,hifactName,metricName,lowfactName)
        }
      else
        {
          if(!(baseLevel %in% lowfactVec))
                {
                  stop("baseLevel is not a level in lowfactName")
                }
          allData <- diffFunction(Diffdata=allData, variable=metricName,
                                  TrxName=trxName, Baseline=baseLevel,
                                  VisitName=lowfactName,ptID=idName,deltaPct=deltaPct)
          colnames(allData) <- c(idName,trxName,lowfactName,metricName)
        }
      allData[[trxName]] <- ordered(allData[[trxName]], trxVector)
    }

# run boxplot without plotting to generate boxplot stats.


  bpFunc <- function(var)
    {
     bp <- boxplot(allData[[metricName]] ~ var,
                    axes=FALSE,
                    main=pTitle,
                    plot=FALSE)

# alter bp Zekai-style to use the selected quantile intervals
      quantile05.values <- tapply(allData[[metricName]],var,
                                  quantile, probs = c(lWhisker), na.rm=TRUE)
      bp$stats[1,] <- quantile05.values
      quantile95.values <- tapply(allData[[metricName]],var,
                                  quantile, probs = c(hWhisker), na.rm=TRUE)
      bp$stats[5,] <- quantile95.values
      quantile25.values <- tapply(allData[[metricName]],var,
                                  quantile, probs = c(lHinge), na.rm=TRUE)
      bp$stats[2,] <- quantile25.values
      quantile75.values <- tapply(allData[[metricName]],var,
                                  quantile, probs = c(hHinge),na.rm=TRUE)
      bp$stats[4,] <- quantile75.values
      std.values <- tapply(allData[[metricName]],var,sd, na.rm=TRUE)
      min.values <- tapply(allData[[metricName]], var,min, na.rm=TRUE)
      max.values <- tapply(allData[[metricName]], var,max, na.rm=TRUE)
      median.values <- tapply(allData[[metricName]],var,median, na.rm=TRUE)
      mean.values <- tapply(allData[[metricName]],var,mean, na.rm=TRUE)
     var.levels <- levels(var)
     return(list(bp=bp,quantile.05.values=quantile05.values,
                 quantile95.values=quantile95.values,
                 quantile25.values=quantile25.values,
                 quantile75.values=quantile75.values,
                 std.values=std.values,
                 min.values=min.values,
                 max.values=max.values,
                 median.values=median.values,
                 mean.values=mean.values,
                 var.levels=var.levels))
    }

  if(!Ropen)
    {
      if(is.null(hifactName))
        {
          bpList <- bpFunc(allData[[lowfactName]]:allData[[trxName]])
        }
      else
        {
          bpList <- bpFunc(allData[[hifactName]]:allData[[lowfactName]]:allData[[trxName]])
        }
    }
  else
    {
      if(is.null(hifactName))
        {
          bpList <- bpFunc(allData[[lowfactName]])
        }
      else
        {
          bpList <- bpFunc(allData[[hifactName]]:allData[[lowfactName]])
        }
    }
 
  bp <- bpList$bp
  quantile05.values <- bpList$quantile05.values
  quantile95.values <- bpList$quantile95.values
  quantile25.values <- bpList$quantile25.values
  quantile75.values <- bpList$quantile75.values
  std.values <- bpList$std.values
  min.values <- bpList$min.values
  max.values <- bpList$max.values
  median.values <- bpList$median.values
  mean.values <- bpList$mean.values      
  bp$stats <- matrix(bp$stats, nrow=5)
  tablestats <- bp$stats

  if(plotMean)
    {
      bp$stats[3, ] <- mean.values
    } 

  # Remove boxes for the baseLevel if delta=TRUE
  if(delta)
    {
      bp$stats[,grep(baseLevel,bp$names)] <- NA
      bp$n[grep(baseLevel,bp$names)] <- ""
      baseLevelNboxes <- length(grep(baseLevel,bp$names))
    }
 
 # finally, draw the plot and add labels
 # Figure out where to place the boxes based on the number of factor levels
  # First create blanks so there will be an empty box between factor level groups
  
  # This needs to be done for bp$stats, bp$n, and bp$names
  boxgroupN <- ifelse(Ropen, ifelse(is.null(hifactN),1,lowfactN), trxN)
  blanks <- seq(from=boxgroupN+1, length=((length(bp$names)/boxgroupN)-1), by=boxgroupN+1)
  statmat <- matrix(NA,nrow=5,ncol=blanks[length(blanks)]+boxgroupN)
    
  # bp$stats
  statmat[,-blanks] <- bp$stats
  bp$stats <- statmat
  # bp$n
  ntemp <- rep(NA,length(blanks)+length(bp$n))
  ntemp[-blanks] <- bp$n
  bp$n <- ntemp
  # bp$names
  namestemp <- rep(NA,length(blanks)+length(bp$names))
  namestemp[-blanks] <- bp$names
  bp$names <- namestemp
  # need to adjust bp$group to keep outliers linked to the correct level
  bp$group <- match(bpList$var.levels[bp$group],bp$names)
  
  if(!plotMean)
    {
      bxp(bp, ylab = yLab,
          xaxt = "n", boxwex=boxWex, medlty="blank",
          outline = pOutliers, axes=FALSE, ylim=yLim)
    }
  else
    {
      bxp(bp, ylab = yLab,
          xaxt = "n", boxwex=boxWex,
          outline = pOutliers, axes=FALSE, ylim=yLim)
    }

  # add median values in points form based on delta or not
  bxPlace <- seq(from=1,to=blanks[length(blanks)]+boxgroupN,by=1)
  boxPlace <- bxPlace[-blanks]
  if(delta)
    {
      if(length(median.values) <= 12)
        {  
          points(x=boxPlace[-c(1:baseLevelNboxes)],
                 y=median.values[-c(1:baseLevelNboxes)], pch=trxPch[1:boxgroupN], cex=0.9)
        }
      else
        {
          points(x=boxPlace[-c(1:baseLevelNboxes)],
                 y=median.values[-c(1:baseLevelNboxes)], pch=trxPch[1:boxgroupN], cex=0.6)
        }
    }
  else
    {
      if(length(median.values) <= 12)
        {
          points(x=boxPlace, y=median.values, pch=trxPch[1:boxgroupN], cex=0.9)
        }
      else
        {
          points(x=boxPlace, y=median.values, pch=trxPch[1:boxgroupN], cex=0.6)
        }
    }

  # get placement for lowfactNames in graph
  lowfctPlace <- tapply(boxPlace,cut(boxPlace,c(0,blanks,boxPlace[length(boxPlace)]+1)),mean) 

  nPlace <- par("usr")[1]
# Get P-Values
  pvtable <- FALSE
  if(printPVals)
    {
      if(is.null(hifactName))
        {
          pvals <- uwPVals(data=allData, factNames=lowfactName, trxName=trxName, pAdjust=pAdjust,
                           metricName=metricName, trxControl=trxControl, pTest=pTest,
                           pairwise=pairwise, pExclude=pExclude, pInclude=pInclude,
                           abbrevN=abbrevN)
        }
      else
        {
          pvals <- uwPVals(data=allData, factNames=c(hifactName,lowfactName), trxName=trxName,
                           metricName=metricName, trxControl=trxControl, pTest=pTest, pAdjust=pAdjust,
                           pairwise=pairwise, pExclude=pExclude, pInclude=pInclude,
                           abbrevN=abbrevN)
        }
      # Remove NaN from being printed
      pvals$pv[pvals$pv=="NaN"] <- ""
      pvals$pv[pvals$pv=="NA"] <- ""

      if(is.null(pInclude[[1]][[1]]))
        {
          pInclude <- NULL
        }
      
      if(pairwise==FALSE & PVinTableOnly==FALSE)
	{
          mtext(paste(pvals$contrast[1]," p",sep=""), side=3, at=nPlace, line=0.1, cex=PValCex)
          mtext(pvals$pv, side=3, at=lowfctPlace, line=0, cex=PValCex)
        }     
      else if(pairwise)
        {
          if(!PVinTableOnly){mtext("p", side=3, at=nPlace, line=0.1, cex=PValCex)}
          if(!is.null(trxControl) & is.null(pInclude))
            {
              if(!PVinTableOnly){mtext(pvals$pv, side=3, at=boxPlace, line=0, cex=PValCex)}
            }
          else  # This encompasses all pairwise or when using pInclude
            {
              if(!PVinTableOnly){mtext("contrast",side=3,at=nPlace,line=0.75, cex=PValCex)}
              pvmat <- cbind("pv"=pvals$pv,"contrast"=pvals$contrast,"levels"=pvals$levels)
              pvmat[,3] <- ordered(pvmat[,3], unique(pvmat[,3]))
              pIncCont <- c()
              pIncPV <- c()
              for(i in 1:nlevels(as.factor(pvmat[,3])))
                {
                  thisLev <- subset(pvmat, pvmat[,3]==as.character(i))
                  
                  if(nrow(thisLev) > trxN)
                    {
                      temppv <- thisLev[,1][1:trxN]
                      tempcon <- thisLev[,2][1:trxN]
                      pvtable <- TRUE
                    }
                  else
                    {
                      temppv <- c(thisLev[,1],rep("",trxN-length(thisLev[,1])))
                      tempcon <- c(thisLev[,2],rep("",trxN-length(thisLev[,2])))
                      pIncPV <- c(pIncPV,temppv)
                      pIncCont <- c(pIncCont,tempcon)
                    }
                  tempcon[temppv==""] <- ""

                  if(!PVinTableOnly)
                    {
                      mtext(temppv,at=boxPlace[((i-1)*trxN+1):(i*trxN)], line=0, cex=PValCex)
                      mtext(tempcon,at=boxPlace[((i-1)*trxN+1):(i*trxN)], line=0.75, cex=PValCex)
                    }
                }
            }
        }
    }
 
 # title
  title(main=pTitle)
  
  # Label bottom axis with sample sizes and factor levels
  mtext(bp$n[!is.na(bp$n)], side=1, line=0, at = boxPlace, cex = LabelCex)
  mtext("n", side=1,line=0, at = nPlace, cex=LabelCex, adj=1)
  
  #legend
  if(Legend)
    {
      legend(LegendLoc, legend=trxVector, pch=trxPch[1:trxN], cex=LegendCex)
      startLine <- 1
    }
  else
    {
      if(!Ropen)
        {
          startLine <- 2
          if(delta)
            {
              mtext(trxVector, side=1, line=1,
                    at=boxPlace[-c(1:baseLevelNboxes)], cex=LabelCex)
            }
          else
            {
              mtext(trxVector, side=1, line=1, at=boxPlace, cex=LabelCex)
            }
        }
      else
        {
          startLine <- 1
          if(delta)
            {
              if(!is.null(hifactName))
                {
                  mtext(lowfactVec, side=1, line=1,
                        at=boxPlace[-c(1:baseLevelNboxes)], cex=LabelCex)
                }
              else
                {
                  mtext(lowfactVec[-1], side=1, line=1,
                        at=boxPlace[-c(1:baseLevelNboxes)], cex=LabelCex)
                }
            }
          else
            {
              mtext(lowfactVec, side=1, line=1, at=boxPlace, cex=LabelCex)
            }
        }
    }
  
   # get placement for hifactNames in graph
  if(!Ropen)
    {
      if(!is.null(hifactName))
        {
          hifctPlace <- tapply(boxPlace,
                               cut(boxPlace,seq(from=0,by=trxN*lowfactN+lowfactN,length=hifactN+1)),
                               mean)
          if(delta)
            {
              mtext(hifactVec[-1], side=1, line=startLine+1, at=hifctPlace[-1], cex=LabelCex)
              mtext(lowfactVec, side=1, line=startLine,
                    at=lowfctPlace[-c(1:baseLevelNboxes/trxN)], cex=LabelCex)
              abline(h=0, lty=2)
            }
          else
            {
              mtext(hifactVec, side=1, line=startLine+1, at=hifctPlace, cex=LabelCex)
              mtext(lowfactVec, side=1, line=startLine, at=lowfctPlace, cex=LabelCex)
            }
        }
      else
        {
          if(delta)
            {
              mtext(lowfactVec[-1], side=1, line=startLine,
                    at=lowfctPlace[-c(1:baseLevelNboxes/trxN)], cex=LabelCex)
              abline(h=0, lty=2)
            }
          else
            {
              mtext(lowfactVec, side=1, line=startLine, at=lowfctPlace, cex=LabelCex)
            }
        }
    }
  else
    {
      if(!is.null(hifactName))
        {
          if(delta)
            {
              mtext(hifactVec[-1], side=1, line=startLine+1, at=lowfctPlace[-1], cex=LabelCex)
              abline(h=0, lty=2)
            }
          else
            {
              mtext(hifactVec, side=1, line=startLine+1, at=lowfctPlace, cex=LabelCex)
            }
        }
    }
  
 # axes
  axis(2, at=axTicks(2), labels = paste(axTicks(2), sep=""), las=2, cex.axis = 0.8)
  
  box()

  table.values <- c()
  if(!is.null(hifactName))
    {
      if(!Ropen)
        {
          upFact <- rep("",length=hifactN*lowfactN*trxN)
          upFact[seq(from=1,to=length(upFact),by=trxN*lowfactN)] <- hifactVec
        }
      else
        {
          upFact <- rep("",length=hifactN*lowfactN)
          upFact[seq(from=1, to=length(upFact), by=lowfactN)] <- hifactVec
        }
      
      table.values <- cbind(upFact)
    }
  if(!Ropen)
    {
      lowFact <- rep("",length=lowfactN*trxN)
      lowFact[seq(from=1,to=length(lowFact),by=trxN)] <- lowfactVec
    }
  else
    {
      lowFact <- lowfactVec
    }
  table.values <- cbind(table.values,lowFact)

  if(!Ropen)
    {
      table.values <- cbind(table.values,
                            "TRT" = rep(trxVector, length=length(table.values[,1])),
                            "N"=bp$n[!is.na(bp$n)], 
                            "Mean"=format(mean.values,digits=2,nsmall=numDec),
                            "SD"=format(std.values,digits=2,nsmall=numDec),
                            "Min"=format(min.values,digits=2,nsmall=numDec),
                            "P5"=format(tablestats[1,],digits=2,nsmall=numDec),
                            "Q1"=format(tablestats[2,],digits=2,nsmall=numDec),
                            "Med"=format(tablestats[3,],digits=2,nsmall=numDec),
                            "Q3"=format(tablestats[4,],digits=2,nsmall=numDec),
                            "P95"=format(tablestats[5,],digits=2,nsmall=numDec),
                            "Max"=format(max.values,digits=2,nsmall=numDec))
    }
  else
    {
      table.values <- cbind(table.values,
                            "N"=bp$n[!is.na(bp$n)], 
                            "Mean"=format(mean.values,digits=2,nsmall=numDec),
                            "SD"=format(std.values,digits=2,nsmall=numDec),
                            "Min"=format(min.values,digits=2,nsmall=numDec),
                            "P5"=format(tablestats[1,],digits=2,nsmall=numDec),
                            "Q1"=format(tablestats[2,],digits=2,nsmall=numDec),
                            "Med"=format(tablestats[3,],digits=2,nsmall=numDec),
                            "Q3"=format(tablestats[4,],digits=2,nsmall=numDec),
                            "P95"=format(tablestats[5,],digits=2,nsmall=numDec),
                            "Max"=format(max.values,digits=2,nsmall=numDec))
    }

#  Format P-Values and Contrasts for Latex table
  if (printPVals)
    {
      if(!pvtable)
        {
          if(!pairwise)
            {
              Contrast <- rep("",length(table.values[,1]))
              pval <- rep("",length(table.values[,1]))
              place <- seq(from=trxN,to=length(Contrast),by=trxN)
              Contrast[place] <- pvals$contrast
              pval[place] <- pvals$pv
            }
          else
            {
              if(!is.null(trxControl) & is.null(pInclude))
                {
                  Contrast <- pvals$contrast
                  pval <- pvals$pv
                }
              else
                {
                  Contrast <- pIncCont
                  pval <- pIncPV
                }
            }
     # Put P-Values and Contrasts into the Latex Table
          table.values <- cbind(table.values, "Contrast"=Contrast, "P-Value"=pval)
        }
      else
        {
          if(!delta)
            {
              pval.table <- matrix(pvmat[,1],nrow=length(unique(pvmat[,2])))
              pval.table <- cbind(unique(pvmat[,2]),pval.table)
              if(!is.null(hifactName))
                {
                  colnames(pval.table) <- c("Contrast",rep(lowfactVec, length(hifactVec)))
                  cgroup <- c("",hifactVec)
                  n.cgroup <- c(1,rep(lowfactN, length(cgroup)-1))
                  cgroup.just <- c("|c|",rep("c",length(cgroup)-2),"c|")
                }
              else
                {
                  colnames(pval.table) <- c("Contrast",lowfactVec)
                  cgroup <- NULL
                  n.cgroup <- NULL
                  cgroup.just <- NULL
                }
            }
          else
            {
              pval.table <- matrix(pvmat[,1][-grep(baseLevel,pvals$levels)],
                                   nrow=length(unique(pvmat[,2])))
              pval.table <- cbind(unique(pvmat[,2]),pval.table)

              if(!is.null(hifactName))
                {
                  colnames(pval.table) <- c("Contrast",rep(lowfactVec, length(hifactVec)-1))
                  cgroup <- c("",hifactVec[-grep(baseLevel, hifactVec)])
                  n.cgroup <- c(1,rep(lowfactN,length(cgroup)-1))
                  cgroup.just <- c("|c|",rep("c",length(cgroup)-2),"c|")
                }
              else
                {
                  colnames(pval.table) <- c("Contrast",lowfactVec[-grep(baseLevel, lowfactVec)])
                  cgroup <- NULL
                  n.cgroup <- NULL
                  cgroup.just <- NULL
                }
            }
          col.just <- c("|l|",rep("c",length(colnames(pval.table))-2),"c|")
          
          pvtexSize <- ifelse(ncol(pval.table) < 10, "normalsize",
                                   ifelse(ncol(pval.table) < 14, "scriptsize","tiny"))
          
          if(is.null(LatexFileName))
            {
              print(pval.table)
              showTab <- FALSE
              warning("LatexFileName is NULL. P-Value Table not saved.")
            }
          else
            {
              pvtabFileName <- paste(strsplit(LatexFileName,",tex"),"PV.tex",sep="")

              ltxPVTbl <- uwLatex(mat=pval.table, file=pvtabFileName,
                                  caption=paste("P-Values for",pTitle),
                                  col.just=col.just, cgroup=cgroup, n.cgroup=n.cgroup,
                                  size=pvtexSize, cgroup.just=cgroup.just)
              print(paste(pvtabFileName, "was created for the p-values"))
            } 
        }
    }

  # Remove baseline data of 0s if delta=TRUE
  if(delta)
    {
      table.values <- table.values[-c(1:baseLevelNboxes),]
    }
  if(!is.null(hifactName))
    {
      colnames(table.values)[1] <- hifacttableName
      colnames(table.values)[2] <- lowfacttableName
      if(Ropen)
        {
          col.just <- c("l","l|",rep("r",10))
        }
      else
        {
          if(printPVals & !pvtable)
            {
              col.just <- c("l","l","l|",rep("r", 10),"|r","r")
            }
          else
            {
              col.just <- c("l","l","l|",rep("r", 10))
            }
        }
    }
  else
    {
      if(is.vector(table.values))
        {
          table.values <- matrix(table.values, nrow=1,
                                 dimnames=list(NULL,names(table.values)))
        }
      colnames(table.values)[1] <- lowfacttableName
      
      if(Ropen)
        {
          col.just <- c("l|",rep("r",10))
        }
      else
        {
          if(printPVals & !pvtable)
            {
              col.just <- c("l","l|",rep("r", 10),"|r","r")
            }
          else
            {
              col.just <- c("l","l|",rep("r", 10))
            }
        }
    }
 
  rownames(table.values) <- NULL
  
  if(is.null(LatexFileName))
    {
      print(table.values)
      showTab <- FALSE
      warning("LatexFileName is NULL. Table not saved.")
    }
  else
    {
      if(is.null(LatexCaption))
        {
          ltxTbl <- uwLatex(mat=table.values, file=LatexFileName, caption=pTitle,
                            col.just=col.just, ...)
        }
      else
        {
          ltxTbl <- uwLatex(mat=table.values, file=LatexFileName,caption=LatexCaption,
                            col.just=col.just, ...)
        }
    }

  if(showTab)
    {structure(list(file=LatexFileName), class='latex')}
  
}
