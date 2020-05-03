##
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


#######################################
# Function uwBarplot 
# Produces bar plot and LaTex table of categorical variables vs. treatment
#######################################



#' Bar Plots for various conditions
#' 
#' Bar graph for one or two categorical variables.  trxName is on the outside
#' of the graph and factorName is on the inside of the graph. uwBar is used to
#' create the bar plot. uwPVals calculates the p-values. uwLatex is used to
#' create a corresponding LaTeX table.
#' 
#' 
#' Additional Functions Required: uwLatex(), uwPVals(), uwBar(), and
#' figureBox()
#' 
#' factorName must be a column name in demoData and a factor or able to be
#' converted to a factor trxName, if given, must be a column name in demoData
#' and a factor or able to be converted to a factor.
#' 
#' It is recommended that factorName is an ordered vector especially when it is
#' an ordinal variable.  This will ensure proper order in the graph and table.
#' 
#' With printPVals==TRUE, see documentation for uwPVals() to see how to use
#' pTest, pairwise, trxControl, and pExclude effectively
#' 
#' With printPVals==TRUE and trxControl=NULL, all two-way comparisons are
#' reported for the levels of trxName.  No adjustments are made to the p-values
#' to account for the multiple testing.  pAdjust can be utilized to make
#' p-value adjustments.
#' 
#' @param demoData data frame containing 1 or 2 factor columns
#' @param demoBarPlot (logical) TRUE plots a bar plot like the old
#' uwDemoBarPlot function in which each factorName level is broken down by
#' trxName.  FALSE plots a traditional uwBarPlot in which each trxName level is
#' broken down by factorName
#' @param trxName (string) Name of the demoData column containing the treatment
#' factor. If NULL then bar plot for just factorName is given
#' @param factorName (string) Name of the demoData column containing the
#' categorical factor.
#' @param factorNames Vector of names to be applied to the factor column. Be
#' careful with order.  Table is created with factorName levels, which can then
#' be switched if factorNames is not NULL.  Length must be the same as number
#' of levels of factorName. If factorName is ordered follow that ordering when
#' listing the order of factorNames, if not then follow alphabetic ordering of
#' levels of factorName.
#' @param cex.names (uwBar) expansion factor for axis names (bar levels)
#' @param pBeside (uwBar)(logical) If TRUE the graph is a stacked bar plot.
#' @param pHoriz (uwBar)(logical) If TRUE the bars are horizontal to the
#' x-axis.
#' @param yLab (uwBar)(string) Label of y-axis.
#' @param xLab (uwBar)(string) Label of x-axis.
#' @param pTitle (string) Title of the graph.
#' @param axLim (c(start, finish)) Gives limit of axis for bar heights. x-axis
#' if pHoriz=TRUE, y-axis if pHoriz=FALSE
#' @param printBarVals (uwBar)(logical) If TRUE the value represented by each
#' bar is printed over the top of the bar. Only if pBeside=TRUE
#' @param digits (positive numeric) Indicates number of decimals to be reported
#' in the graph and in the table
#' @param lined (uwBar)(logical) TRUE has levels of bars alternate between
#' solid and lined bars.
#' @param Percent (logical) If TRUE the graphs show percentages of patients
#' instead of counts.
#' @param barNamesLas (0,1,2,or 3). Changes the verticality of the treatment
#' names for bar names and numeric axis. 1 and 2 are best for reporting
#' @param barNamesSeq (uwBar)(integer) Number indicating which bar labels to
#' print.  This number is given as the "by" attribute to seq().  So every
#' barNameSeq would be printed.  Default is 1, which will print all bar labels.
#' Helpful when bar labels overlap when printing all of them.
#' @param barNamesAngle (uwBar)(integer) Number between 0 and 360 indicating
#' how the margin names should be rotated.  Useful when margin names are long
#' strings so you can angle them to all be seen
#' @param printPVals (uwPVals)(logical) If TRUE, p-values indicated by the
#' attributes for uwPVals are printed in the margin of the plot.
#' @param pTest (uwPVals)(printPVals=TRUE) Chose type of comparison test
#' @param abbrevN (uwPVals)(integer) indicating how many letters should
#' abbreviation of the treatments levels be when reported in margins.
#' @param pairwise (uwPVals)(logical) If TRUE, pairwise comparisons should be
#' made between treatment levels.  If FALSE, multi-level test occurs between
#' all levels of the treatment (i.e. ANOVA)
#' @param pAdjust (uwPVals)(pairwise=TRUE) NULL for none, "h" for "holm", or
#' "b" for "bonferroni". P-value adjustment methods.
#' @param trxControl (uwPVals)(pairwise=TRUE) If NULL, all two-way tests will
#' be examined.  If a treatment level is indicated here as the control level,
#' then only two-way comparisons with the control and non-control levels will
#' be made.
#' @param pExclude (uwPVals)(pairwise=TRUE) If trxControl is NULL then this
#' should be a vector of treatment levels, looked at two at a time, that
#' indicate that the p-value for that contrast should NOT be reported.  Suppose
#' the treatment levels are A,B,C,D and you do not want the comparisons for
#' B.C., B.D., and C.D..  pExclude should look like this.
#' pExclude=c("B","C","B","D","C","D"). If trxControl is defined then this
#' should be a vector of non-control treatment levels that will be looked at
#' one at a time and that non-control's contrast will NOT be reported
#' @param pInclude (uwPVals)(pairwise=TRUE) Gives the user the ability to
#' specify which contrasts they would like to get p-values for.  The ability to
#' combine treatment levels for a contrast is possible.  See help(uwPVals) for
#' more details.
#' @param pStrWrap (logical) If TRUE, creates two rows of p-value string on the
#' top margin instead of one.  Useful for many reported p-values.
#' @param pValCex Number between 0 and 1. Size of the p-value font in the top
#' margin.
#' @param titleCex (figureBox) Number between 0 and 1.  Size of the title font
#' @param Legend (uwBar)(logical) If TRUE, puts a legend in the graph
#' @param LegendLoc (uwBar)(string)(Legend=TRUE). Gives the location in the
#' graph where the legend will be located. One of these must be given.
#' "topleft", "top", "topright", "left", "center", "right", "bottomleft",
#' "bottom", or "bottomright"
#' @param LegendCex Size of legend. Number between 0 and 1, 1=full size, 0=no
#' size.
#' @param LatexFileName (uwLatex)(string) Giving the folder and file(ending in
#' .tex) for the LaTeX table to be saved. If NULL, table will be printed, but
#' not saved.
#' @param LatexCaption (uwLatex)(string) Giving the caption for each LaTeX
#' table.  If NULL, defaults to pTitle
#' @param trxColname (uwLatex)(string) Name for first column, which is like a
#' title for the rows.  The default is trxName if !is.null(trxName), or
#' factorName if is.null(trxName).
#' @param ... Any other argument that can be passed to uwLatex()
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @examples
#' 
#' 
#' TRT <- rep(c("A","B","C"), 30)
#' AE <-ordered(sample(c("None","Mild","Moderate","Severe"),90,replace=TRUE),
#'              c("None", "Mild", "Moderate", "Severe"))
#' data <- data.frame(TRT,AE)
#' 
#' layout(matrix(c(1,1,2,2,3,3,4,4),nrow=2,byrow=TRUE))
#' uwBarPlot(demoData=data,trxName=NULL,factorName="AE",Legend=TRUE,demoBarPlot=FALSE,pHoriz=TRUE,pBeside=TRUE,
#'           xLab="Number of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=60,cex.names=0.8,
#'           lined=FALSE,Percent=FALSE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
#'           LatexFileName=NULL,LegendLoc="topright")
#' 
#' uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=TRUE,pHoriz=TRUE,
#'           xLab="Number of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=0,
#'           lined=FALSE,Percent=FALSE,barNamesLas=0,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
#'           LatexFileName=NULL,LegendLoc="topright",pStrWrap=TRUE)
#' 
#' uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=FALSE,pHoriz=FALSE,
#'           yLab="Percent of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=0,
#'           lined=FALSE,Percent=TRUE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
#'           LatexFileName=NULL,LegendLoc="topleft")
#' 
#' uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=TRUE,pHoriz=FALSE,
#'           yLab="Percent of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=45,
#'           lined=TRUE,Percent=TRUE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
#'           LatexFileName=NULL,LegendLoc="topleft")
#' 
uwBarPlot <- function(demoData,
                      demoBarPlot=FALSE,
                      trxName=NULL,
                      factorName,
                      factorNames=NULL,
                      cex.names=1,
                      pBeside=TRUE,
                      pHoriz=FALSE,
                      yLab=NULL,
                      xLab=NULL,
                      pTitle=NULL,
                      axLim=NULL,
                      printBarVals=TRUE,
                      digits=1,
                      lined=TRUE,
                      Percent=TRUE,
                      barNamesLas=1,
                      barNamesSeq=1,
                      barNamesAngle=0,
                      printPVals=TRUE,
                      pTest=c("wilcox","fisher","chisq"),
                      abbrevN=1,
                      pairwise=TRUE,
                      pAdjust=NULL,
                      trxControl=NULL,
                      pExclude=NULL,
                      pInclude=list(list(NULL,NULL)),
                      pStrWrap=FALSE,
                      pValCex=0.7,
                      titleCex=1,
                      Legend=TRUE,
                      LegendLoc="topright",
                      LegendCex=0.8,
                      LatexFileName=NULL,
                      LatexCaption=NULL,
                      trxColname=NULL,
                      ...)
{
  require(Hmisc)

  if(!is.data.frame(demoData)){stop("demoData is not a data.frame")}

  if(!is.null(trxName))
    {
      # Make sure trxName is a column in demoData
      if(!(trxName %in% colnames(demoData)))
        stop("trxName is not a colname of demoData")
      # Remove any entries with "NA" in the treatment variable column.
      demoData <- demoData[!is.na(demoData[[trxName]]),]
 
      if (!is.factor(demoData[[trxName]]))
        demoData[[trxName]] <- factor(demoData[[trxName]])
    }
  
  if (!is.factor(demoData[[factorName]]))
      demoData[[factorName]] <- factor(demoData[[factorName]])
  catFactor <- demoData[[factorName]]
  catVector <- levels(catFactor)
  
  if(is.null(trxName))
    {
      printPVals <- FALSE  # no p values on open reports
      Legend <- FALSE  # no legend necessary for open reports
      legText <- NULL

      tab <- table(catFactor)
      if(!is.null(factorNames))
        {
          if(length(catVector) != length(factorNames))
            {stop("Length of factorNames != number of levels in factorName")}
          names(tab) <- factorNames
        }
      totN <- sum(tab)
      outLevels <- names(tab)
      TexTab <- tab
      inLevels <- NULL
      PctTab <- round(100*(tab/sum(tab)), digits)
      TexPctTab <- PctTab
      if(Percent)  # change to Percentages if necessary
          tab <- PctTab
    }
  else
    {
      trxFactor <- demoData[[trxName]]
      trxVector <- levels(trxFactor)
  ##################################################  First differences between this and Demo
      if(demoBarPlot)
        {
          tab <- table(trxFactor, catFactor)
          trxN <- apply(tab,1,sum) # Get treatment totals
          if(!pBeside)
            {
              pBeside <- TRUE
              warning("for demoBarPlot pBeside must be TRUE")
            }
        }
      else
        {
          tab <- table(catFactor, trxFactor)
          trxN <- apply(tab,2,sum) # Get column totals
        }
 ####################################################     
      if(!is.null(factorNames))
        {
          if(length(catVector) != length(factorNames))
            {stop("Length of factorNames != number of levels in factorName")}
          if(demoBarPlot)
            colnames(tab) <- factorNames
          else
            rownames(tab) <- factorNames
        }
      TexTab <- if(demoBarPlot) tab else t(tab)
      
      # Find p-values
      if(printPVals)
        {
          pvals <- uwPVals(data=demoData, factNames=factorName, trxName=trxName,pAdjust=pAdjust,
                           pTest=pTest, trxControl=trxControl, pairwise=pairwise,
                           pExclude=pExclude, pInclude=pInclude, abbrevN=abbrevN)
        }
      # Get percent table: row if demoBarPlot and columne if !demoBarPlot
      PctTab <- round(100*prop.table(tab,ifelse(demoBarPlot,1,2)),digits)
      TexPctTab <- if(demoBarPlot) PctTab else t(PctTab)
      
      if(Percent)  # change to Percentages if necessary
          tab <- PctTab
    }
  
  if(pHoriz)
    {
      mgp.y <- c(3,1,0)
      mgp.x <- c(1.5,0.5,0)
    }
  else
    {
      mgp.y <- c(1.5,0.5,0)
      mgp.x <- c(1.5,0.75,0)
    }
    # Digits should be zero if percentage isn't being printed, but allows percentage to have decimals in Latex table
  uwb <- uwBar(table=tab,pBeside=pBeside,pHoriz=pHoriz,lined=lined,
               axLim=axLim,yLab=yLab,xLab=xLab,printBarVals=printBarVals,
               Legend=Legend,LegendLoc=LegendLoc,LegendCex=LegendCex,
               mgp.y=mgp.y,mgp.x=mgp.x,cex.names=cex.names,barNamesLas=barNamesLas,
               barNamesSeq=barNamesSeq,barNamesAngle=barNamesAngle,digits=ifelse(Percent,digits,0))
  bp <- uwb[[1]]
  tab <- uwb[[2]]
  a <- uwb[[3]]

  fb <- figureBox(boxPlot=FALSE,demoBarPlot=demoBarPlot,pHoriz=pHoriz,pTitle=pTitle,
                  titleCex=titleCex,printPVals=printPVals,pStrWrap=pStrWrap)

  if(is.null(trxName))
    {  #  Add sample size
      if(pHoriz)
        text(x=par("usr")[1]-0.4*fb[[2]],y=mean(bp),label=paste("N = ",totN,sep=""),cex=0.7,xpd=TRUE,srt=90)
      else
        text(x=mean(bp),y=par("usr")[3]-0.4*fb[[1]],label=paste("N = ",totN,sep=""),cex=0.7,xpd=TRUE)
    }
  else
    {
      if(printPVals)
        {
          pv <- pvals$pv[pvals$pv!=""]
          contrast <- pvals$contrast[pvals$contrast!=""]
          pString <- paste(contrast,"=",pv, sep="",collapse="   ")
          if(!pairwise){pStrWrap==FALSE}
          if(pStrWrap)
            {
              stwrap <- strwrap(pString, 5)
              half <- floor(length(stwrap)/2)
              pString1 <- paste(stwrap[1:half], collapse="  ")
              pString2 <- paste(stwrap[(half+1):length(stwrap)], collapse="  ")
              mtext(pString1, side=3, line=.75, cex=pValCex)
              mtext(pString2, side=3, line=0, cex=pValCex)
            }
          else
            {
              mtext(pString, side=3, line=0, cex=pValCex)
            }
        }
      abbrevTrx <- abbreviate(trxVector,abbrevN)
      if(pHoriz)
        {
          textLab <- if(demoBarPlot){paste(paste("n",abbrevTrx," = ",trxN,sep=""),collapse="     ")}else{rev(trxN)}
          if(!demoBarPlot)
            {
              text(x=par("usr")[1]-0.4*fb[[2]],y=par("usr")[4]+0.4*fb[[1]],label="N",xpd=TRUE,cex=0.7)
              text(y=if(pBeside){apply(bp,2,mean)}else{bp},x=par("usr")[1]-0.4*fb[[2]],labels=textLab,cex=0.7,srt=90,xpd=TRUE)
            }
          else
            text(y=mean(bp),x=par("usr")[1]-0.4*fb[[2]],labels=textLab,cex=0.7,srt=90,xpd=TRUE)
          
        }
      if(!pHoriz)
        {
          textLab <- if(demoBarPlot){paste(paste("n",abbrevTrx," = ",trxN,sep=""),collapse="     ")}else{trxN}
          if(!demoBarPlot)
            {
              text(x=par("usr")[1]-0.4*fb[[2]],y=par("usr")[3]-0.4*fb[[1]],label="N",xpd=TRUE,cex=0.7)
              text(x=if(pBeside){apply(bp,2,mean)}else{bp},y=par("usr")[3]-0.4*fb[[1]],labels=textLab,cex=0.7,xpd=TRUE)
            }
          else
            text(x=mean(bp),y=par("usr")[3]-0.4*fb[[1]],labels=textLab,cex=0.7,xpd=TRUE)
        }
    }
  
# Create LaTeX table
  if(is.null(trxName))
    {
      cgroup <- NULL
      n.cgroup <- NULL
      LatexFact <- c(names(TexTab), "Tot. N")
      N <- c(TexTab, sum(TexTab))
      Pct <- c(TexPctTab, "")
      LatexTab <- cbind("Factor"=LatexFact, N, Pct)
      rownames(LatexTab) <- NULL
    }
  else
    {
      if(!printPVals)
        rowLength <- length(trxVector)
      else
        rowLength <- max(c(length(trxVector), length(contrast)))
      
      LatexTRT <- rep("",rowLength)
      LatexTRT[1:(length(trxVector))] <- trxVector
      LatexTotN <- rep("",rowLength)
      LatexTotN[1:(length(trxVector))] <- apply(TexTab,1,sum)

      LatexCols <- c()
      for(i in 1:length(catVector))
        {
          N <- rep("",rowLength)
          Pct <- rep("",rowLength)
          N[1:nrow(TexTab)] <- TexTab[,i]
          Pct[1:nrow(TexPctTab)] <- TexPctTab[,i]
          LatexCols <- cbind(LatexCols, N, Pct)
        }
      if(printPVals)
        {
          Contst <- rep("",times=rowLength)
          Contst[1:(length(contrast))] <- contrast
          PValues <- rep("", times=rowLength)
          PValues[1:(length(pv))] <- pv
          LatexTab <- cbind(LatexTRT, "Tot. N"=LatexTotN, LatexCols,
                            "Contrast"=Contst, "P-Value"=PValues)
          cgroup <- c("", "", colnames(TexTab), "","")
          n.cgroup <- c(1,1,rep(2, length(colnames(TexTab))),1,1)
        }
      else
        {
          LatexTab <- cbind(LatexTRT, "Tot. N"=LatexTotN, LatexCols)
          cgroup <- c("","", colnames(TexTab))
          n.cgroup <- c(1,1, rep(2, length(colnames(TexTab))))
        } 
    }
  if(is.null(trxColname))
          trxColname <- ifelse(is.null(trxName),"",trxName)
        
      colnames(LatexTab)[1] <- trxColname
  if(is.null(LatexCaption))
      LatexCaption <- pTitle
  
  if(is.null(LatexFileName))
    {
      print(LatexTab)
      warning("LatexFileName is NULL.  Table was not saved.")
    }
  else
    {
      w <- uwLatex(mat=LatexTab,file=LatexFileName,
                   cgroup=cgroup,n.cgroup=n.cgroup,caption=LatexCaption, ...)
    }  
}
