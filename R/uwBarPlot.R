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
