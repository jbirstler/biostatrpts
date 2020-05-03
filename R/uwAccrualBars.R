## Copyright (C) 2004 Frontier Science & Technology
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



uwAccrualBars <- function(accData,
                          dateName,
                          trxName=NULL,
                          interval=c("months","days","years"),
                          startDate=NULL,
                          currentDate=NULL,
                          pTitle="Accrual Over Time",
                          yLab=NULL,
                          xLab=NULL,
                          yLim=NULL,
                          pHoriz=FALSE,
                          pBeside=TRUE,
                          linedbars=FALSE,
                          printBarVals=TRUE,
                          Legend=FALSE,
                          LegendLoc="topright",
                          LegendCex=0.8,
                          cex.names=1,
                          titleCex=1,
                          barNamesLas=1,
                          barNamesSeq=1,
                          barNamesAngle=0)
{

require(chron)
# Remove any lines of data that have missing dateName
accData <- accData[!is.na(accData[[dateName]]),]
# If trxName is NULL then Legend must be FALSE
if(is.null(trxName) & Legend)
  {
    print("if trxName is NULL Legend must be FALSE")
    Legend <- FALSE
  }
# Try to set default of interval to be months, doesn't cover all situations
interval <- ifelse(length(interval) >= 1,interval[1],"months")

if(!(interval %in% c("d","m","y") | interval %in% c("days","months","years") |
     interval %in% c("day","month","year")))
  {
    print("interval is not a valid string defaults to months")
    interval <- "months"
  }
# reduce interval to 1 character for ease of checking
interval <- substr(interval,1,1)

# check to make sure dateName is a date object
if(!("dates" %in% class(accData[[dateName]])))
  {
    warning("Cohercing accData[[dateName]] to a dates class of m/d/y")
    accData[[dateName]] <- chron(as.character(accData[[dateName]]),format=c(dates="m/d/y"))
  }

if(is.null(startDate))
      startDate <- min(accData[[dateName]])
  else
    if(startDate > min(accData[[dateName]]))
      stop(paste("startDate is greater than min Date in accData[[dateName]]:",
                 min(accData[[dateName]]),sep=""))
  if(is.null(currentDate))
    currentDate <- max(accData[[dateName]])
  else
    if(currentDate < max(accData[[dateName]]))
      stop(paste("curentDate is less than max Date in accData[[dateName]]:",
                 max(accData[[dateName]]),sep=""))

# create time table for plotting in barplot
if(interval=="d")
  {
    #timeseq <- seq.dates(min(accData[[dateName]]),max(accData[[dateName]]),1)
    timeseq <- seq.dates(startDate,currentDate,1)
    if(is.null(trxName))
      {
        #timetab <- table(cut(accData[[dateName]],"days"))
        timetab <- table(cut(c(accData[[dateName]],startDate,currentDate),"days"))
        names(timetab) <- as.character(timeseq)
        # Subtract out the entry of artificial dates startDate and currentDate
        timetab[1] <- timetab[1]-1
        timetab[length(timetab)] <- timetab[length(timetab)]-1
      }
    else
      {
        #timetab <- table(accData[[trxName]],cut(accData[[dateName]],"days"))
        timetab <- table(c(accData[[trxName]],NA,NA),cut(c(accData[[dateName]],startDate,currentDate),"days"))
        colnames(timetab) <- as.character(timeseq)
        rownames(timetab) <- levels(accData[[trxName]])
      }
   }
else if(interval=="m")
  {
    if(is.null(trxName))
      {
        #timetab <- table(cut(accData[[dateName]],"months"))
        timetab <- table(cut(c(accData[[dateName]],startDate,currentDate),"months"))
        timetab[1] <- timetab[1]-1
        timetab[length(timetab)] <- timetab[length(timetab)]-1
      }
    else
      {
        timetab <- table(c(accData[[trxName]],NA,NA),cut(c(accData[[dateName]],startDate,currentDate),"months"))
        rownames(timetab) <- levels(accData[[trxName]])
      }
  }
else
  {
    if(is.null(trxName))
      {
        #timetab <- table(cut(accData[[dateName]],"years"))
        timetab <- table(cut(c(accData[[dateName]],startDate,currentDate),"years"))
        timetab[1] <- timetab[1]-1
        timetab[length(timetab)] <- timetab[length(timetab)]-1
      }
    else
      {
        timetab <- table(c(accData[[trxName]],NA,NA),cut(c(accData[[dateName]],startDate,currentDate),"years"))
        rownames(timetab) <- levels(accData[[trxName]])
      }
  }

if(is.null(trxName))
    pBeside <- TRUE

if(pHoriz)
  {
    mgp.y <- c(3,0.5,0)
    mgp.x <- c(1.5,0.5,0)
  }
else
  {
    mgp.y <- c(1.5,0.5,0)
    mgp.x <- c(1.5,0.5,0)
  }

uwb <- uwBar(timetab,pBeside=pBeside,pHoriz=pHoriz,lined=linedbars,
             axLim=yLim,yLab=yLab,xLab=xLab,printBarVals=printBarVals,
             Legend=Legend,LegendLoc=LegendLoc,LegendCex=LegendCex,
             mgp.y=mgp.y,mgp.x=mgp.x,cex.names=cex.names,
             barNamesLas=barNamesLas,barNamesSeq=barNamesSeq,barNamesAngle=barNamesAngle,digits=0)

# put box around title  TAKEN FROM figureBox.R
  UperL <- ((par("usr")[4]-par("usr")[3])/par("pin")[2])/(par("mar")[3]/par("mai")[3])
  rect(par("usr")[1],par("usr")[4],par("usr")[2],par("usr")[4]+1.2*UperL,xpd=TRUE)
# print title
  title(main=pTitle,line=0.3,cex.main=titleCex,font.main=1)

}
