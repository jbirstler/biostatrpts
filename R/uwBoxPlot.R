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

############################
# Function uwBoxPlot 
############################

uwBoxPlot <- function(data, trxName=NULL, 
                      metricName,
                      LatexFileName=NULL,
                      pTitle=NULL,
                      yLab=NULL,
                      xLab=NULL,
                      plotMean=TRUE, pOutliers=TRUE,
                      boxWex=0.75,
                      printPVals=TRUE,
                      pTest=c("wilcox","t.test"),
                      abbrevN=1,
                      pairwise=TRUE,
                      pAdjust=NULL,
                      trxControl=NULL,
                      pExclude=NULL,
                      pInclude=list(list(NULL,NULL)),
                      yLim=NULL,
                      pStrWrap=FALSE,
                      pValCex=0.7,
                      titleCex=1,
                      LatexCaption=NULL,
                      firstCol.name=NULL,
                      digits=1,
                      cex.names=0.7,
                      ...)
  {

    if(!is.null(pInclude[[1]][[1]]) & !is.null(trxControl))
      {
        trxControl <- NULL
        warning("trxControl is ignored when pInclude is defined, so trxControl set to NULL")
      }
    
 # Remove records that have an "NA" in the metric column
    data <- data[!is.na(data[[metricName]]),]

  # strip the input dataset down to only the relevant columns
    if(is.null(trxName))
      {
        data <- subset(data,select=metricName)
        printPVals <- FALSE
      }
    else
      {
        data <- subset(data,select = c(trxName, metricName))
        data[[trxName]] <- factor(data[[trxName]])  # re-factor
      }

    uwbx <- uwBox(data=data,trxName=trxName,metricName=metricName,yLab=yLab,xLab=xLab,yLim=yLim,
                  boxWex=boxWex,plotMean=plotMean,pOutliers=pOutliers)

# PVALUES  

    if(printPVals)
      {
        pvals <- uwPVals(data=data, metricName=metricName, trxName=trxName, pAdjust=pAdjust,
                         trxControl=trxControl, pTest=pTest, pairwise=pairwise,
                         pExclude=pExclude, pInclude=pInclude, abbrevN=abbrevN)
      }

  # add boxes around title, p-values, and sample sizes
    fb <- figureBox(boxPlot=TRUE,demoBarPlot=FALSE,pHoriz=FALSE,pTitle=pTitle,titleCex=titleCex,
                    printPVals=printPVals,pStrWrap=pStrWrap)
    
  # print p-values at top of page
 
    if (printPVals)
      {
        if((pairwise & is.null(trxControl))) #this will also include when pInclude is used
          {
            pv <- pvals$pv[pvals$pv!=""]
            contrast <- pvals$contrast[pvals$contrast!=""]
            pString <- paste(paste("p",contrast,sep=""),"=",pv,sep=" ",collapse="   ")
            if(pStrWrap)
              {
                stwrap <- strwrap(pString, 5)
                half <- floor(length(stwrap)/2)
                pString1 <- paste(stwrap[1:half], collapse="  ")
                pString2 <- paste(stwrap[(half+1):length(stwrap)], collapse="  ")
                mtext(pString1, side=3, line=1, cex=pValCex)
                mtext(pString2, side=3, line=0, cex=pValCex)
              }
            else
              {
                mtext(pString, side=3, line=0, cex = pValCex)
              }
          }
        else if(pairwise & !is.null(trxControl))
            mtext(pvals$pv, side=3,line=0, at=c(1:length(uwbx$n)), cex=pValCex)
          
        else if(!pairwise)
            mtext(paste(pvals$contrast,"=",pvals$pv),side=3, cex=pValCex)
      }    
    # Label bottom axis with trx labels
    if(!is.null(trxName))
      {
        if(length(grep("\n",levels(data[[trxName]])))==0)
          mtext(levels(data[[trxName]]), side=1, line=1, at = c(1:length(uwbx$n)), cex = cex.names)
        else
          mtext(levels(data[[trxName]]), side=1, line=2, at = c(1:length(uwbx$n)), cex = cex.names)
      }
    # Label bottom axis with sample sizes
    text(x=par("usr")[1]-0.4*fb[[2]],y=par("usr")[3]-0.4*fb[[1]],label="N",xpd=TRUE,cex=0.7)
    text(x=1:length(uwbx$n),y=par("usr")[3]-0.4*fb[[1]],labels=uwbx$n, cex = 0.7,xpd=TRUE)
    
    axis(2, at=axTicks(2), tcl=-0.2, labels = paste(axTicks(2), sep=""), las=2, cex.axis = 0.8,
         mgp=c(3,1,0))
  
    box()

  # put out basic stat table
    cgroup <- NULL
    n.cgroup <- NULL
    cgroup.just <- NULL

    if (is.null(trxName))
      {
        LatexTab<-cbind("temp"=metricName,
                        "N"=uwbx$n,
                        "Mean"=rounds(uwbx$mean,digits=digits),
                        "SD"=rounds(uwbx$sd,digits=digits),
                        "Min"=rounds(uwbx$min,digits=digits),
                        "Q1"=rounds(uwbx$q25,digits=digits),
                        "Median"=rounds(uwbx$median,digits=digits),
                        "Q3"=rounds(uwbx$q75,digits=digits),
                        "Max"=rounds(uwbx$max,digits=digits))
        colnames(LatexTab)[1] <- ""
      }
    else
      {
        LatexTab<-cbind(levels(data[[trxName]]),
                        "N"=uwbx$n,
                        "Mean"=rounds(uwbx$mean,digits=digits),
                        "SD"=rounds(uwbx$sd,digits=digits),
                        "Min"=rounds(uwbx$min,digits=digits),
                        "Q1"=rounds(uwbx$q25,digits=digits),
                        "Median"=rounds(uwbx$median,digits=digits),
                        "Q3"=rounds(uwbx$q75,digits=digits),
                        "Max"=rounds(uwbx$max,digits=digits))
  
        if (printPVals)
          {
            rowLength <- max(nlevels(data[[trxName]]), length(pvals$contrast))
            if(nrow(LatexTab) != rowLength)
              {
                diff <- rowLength - nrow(LatexTab)
                for(i in 1:diff)
                  {
                    LatexTab <- rbind(LatexTab, rep("", times=ncol(LatexTab)))
                  }
              }            
            LatexTab <- cbind(LatexTab,
                              "Contrast"=rep("",nrow(LatexTab)),
                              "P-Value"=rep("",nrow(LatexTab)))
            LatexTab[1:length(pvals$contrast),10] <- pvals$contrast
            LatexTab[1:length(pvals$contrast),11] <- pvals$pv
          }
        
        if(is.null(firstCol.name))
            firstCol.name <- trxName
          
        colnames(LatexTab)[1] <- firstCol.name 
      }
    
    if(is.null(LatexCaption))
        LatexCaption <- pTitle
    
    if(is.null(LatexFileName))
      {
        print(LatexTab)
        warning("LatexFileName is NULL. Table not saved.")
      }
    else
      {
        w <- uwLatex(mat=LatexTab, file=LatexFileName, cgroup=cgroup,
                     caption=LatexCaption,  n.cgroup=n.cgroup, ...)
      }    
  }
