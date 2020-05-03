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

#############################
# Produce Survival graph
#############################

uwSurvival <- function(survData,
                       statusName,
                       trxName,
                       trxControl,                  
                       timeName,
                       fun=NULL,
                       color=NULL,
                       pTitle="Survival by Treatment",
                       yLab="Percent of Survival Probability",
                       xLab="Days from Randomization",
                       yLim=NULL,
                       markCensor=FALSE,
                       Legend=TRUE,
                       LegendLoc="topright",
                       LegendCex=0.8,
                       printPVals=TRUE,
                       pValCex=0.6,
                       abbrevN=1,
                       printSS=TRUE,
                       SSinPlot=FALSE,
                       SScex=0.5,
                       Ropen=FALSE,
                       firstCol.name=NULL,
                       GraphSSfreq=1,
                       TableSSfreq=GraphSSfreq,
                       LatexFileName="survival.tex",
                       LatexCaption=NULL,
                       numDec=1,
                       cexAxis=0.7,
                       confInt=FALSE,
                       ...) {
# GraphSSfreq is an integer value specifying the number of times the nRisk values # should be given in the survival graph
 
# create a vector of milestone days from start of treatment
# based on a fixed interval plus any arbitrarily selected days

require(survival)
require(chron)  

# remove any data rows for missing values in trxName
survData <- survData[!is.na(survData[[trxName]]),]

if(!printSS)
  {
    SSinPlot <- FALSE
  }

if (Ropen) {  # effectively change all treatment to control/placebo
  survData[[trxName]] <- trxControl
  survData[[trxName]] <- factor(survData[[trxName]])
}

zeroDate <- 0
maxDate <- max(survData[[timeName]], na.rm=TRUE)

maxDays <- maxDate * 1.1  # arbitrary expansion factor
uneven <- FALSE

mileStones <- seq(zeroDate, maxDate, by=GraphSSfreq)

if(mileStones[length(mileStones)] != maxDate)
  {
    mileStones <- c(mileStones, maxDate)
    uneven <- TRUE
  }

msDays <- round(mileStones,numDec)
numTrx <- length(levels(survData[[trxName]]))
trxVector <- levels(survData[[trxName]])

# Create "Surv" object for analysis
survObj <- Surv(survData[[timeName]], survData[[statusName]])
sForm <- as.formula(paste("survObj ~ ", trxName))

# summary of surv object only shows the days where events occurred, whereas
# we want to show all dates, so we don't use summary.survfit

#dfSummFit <- data.frame(time = summFit$time, n.risk = summFit$n.risk,
#			n.event = summFit$n.event, surv = summFit$surv,
#			std.err = summFit$std.err, lower = summFit$lower,
#			upper = summFit$upper, strata = summFit$strata)

#require(Hmisc)

###############################################################
# Create a survival table for each treatment, each of which
# has its own strata in the survfit object 
###############################################################

if (Ropen){
  fit <- survfit(sForm, data=survData)
# to keep conf interval lines from appearing on graph, need conf.type=none
  fitGraph <- survfit(sForm, data=survData)
} else {
  fit <- survfit(sForm, data=survData)
  fitGraph <- fit
}

# fit<-survfit(Surv(sdays,status) ~ ttreat, data=test, type="kaplan-meier", conf.type="none")

startStrata <- 1

append <- FALSE  # for first table
newPage <- NULL
if(length(LatexFileName) != length(trxVector))
  {
    stop("length(LatexFileName) != length(trxVector)")
  }
if(is.null(LatexCaption))
  {
    LatexCaption <- rep("Survival Summary Table", times=length(trxVector))
  }
for (si in 1:length(trxVector)) {
  if (Ropen){
    endStrata <- length(fit$n.risk)
  } else {
    nStrata <- fit$strata[si]
    endStrata <- startStrata + nStrata - 1
  }
 
# survfit returns a list consisting of summary values as of each day when an event occurred.
# Specifically, time, n.risk, n.event, survival, std.err, lower CI, and upper CI
# split by strata (trx types).
# For the table, group them into 30-day intervals.

  latexTable <- cbind("Days"  = fit$time[seq(startStrata,endStrata,by=TableSSfreq)],
                      "Number\nat Risk" = fit$n.risk[seq(startStrata,endStrata,by=TableSSfreq)],
                      "Number\nof Events" = fit$n.event[seq(startStrata,endStrata,by=TableSSfreq)],
                      "Survival %" = format(100 * fit$surv[seq(startStrata,endStrata,by=TableSSfreq)],
                        digits=2, nsmall=2),
                      "Std\nError" = format(100 * fit$std.err[seq(startStrata,endStrata,by=TableSSfreq)],
                        digits=2, nsmall=2),
                      "Lower" = format(100 * fit$lower[seq(startStrata,endStrata,by=TableSSfreq)],
                        digits=2, nsmall=2),
                      "Upper" = format(100 * fit$upper[seq(startStrata,endStrata,by=TableSSfreq)],
                        digits=2, nsmall=2))

  startStrata <- endStrata+1

   if(!is.null(firstCol.name))
    {
      colnames(latexTable)[1] <- firstCol.name
    }
    
      uwLatex(mat=latexTable, file=LatexFileName[si], caption=LatexCaption[si], ...)

}      
#################
# end of survival summary table
#################
defaultmar <- par("mar")
par(mar = c(8, 8, 5, 3) + 0.1)
color <- ifelse(is.null(color),paste("gray",ceiling(seq(from=10,to=80,length=numTrx))),color)
xBase <- .02 * maxDays  # this will be used to place graph labels
if(SSinPlot)
  {
    if(!is.null(fun))
      {
        yLim <- ifelse(rep(is.null(yLim),2),c(-1,100),yLim)
        pFit <- plot(fitGraph, mark.time=markCensor, lty = c(1:numTrx), 
                     yscale=100,
                     conf.int=confInt,
                     main = pTitle,
                     ylab= yLab,
                     xlab= xLab,
                     xlim=c(0 - xBase, 1.02 * maxDays),
                     ylim=yLim,
                     axes=FALSE,
                     fun=fun,
                     col=color)
      }
    else
      {
        yLim <- ifelse(rep(is.null(yLim),2),c(-1,100),yLim)
        pFit <- plot(fitGraph, mark.time=markCensor, lty = c(1:numTrx), 
                     yscale=100,
                     conf.int=confInt,
                     main = pTitle,
                     ylab= yLab,
                     xlab= xLab,
                     xlim=c(0 - xBase, 1.02 * maxDays),
                     ylim=yLim,
                     axes=FALSE,
                     col=color)
      }
  }
else
  {
    if(!is.null(fun))
      {
        yLim <- ifelse(rep(is.null(yLim),2),c(-10,100),yLim)
        pFit <- plot(fitGraph, mark.time=markCensor, lty = c(1:numTrx), 
                     yscale=100,
                     conf.int=confInt,
                     main = pTitle,
                     ylab= yLab,
                     xlab= xLab,
                     xlim=c(0 - xBase, 1.02 * maxDays),
                     ylim=yLim,
                     axes=FALSE,
                     fun=fun,
                     col=color)
      }
    else
      {
        yLim <- ifelse(rep(is.null(yLim),2),c(-10,100),yLim)
        pFit <- plot(fitGraph, mark.time=markCensor, lty = c(1:numTrx), 
                     yscale=100,
                     conf.int=confInt,
                     main = pTitle,
                     ylab= yLab,
                     xlab= xLab,
                     xlim=c(0 - xBase, 1.02 * maxDays),
                     ylim=yLim,
                     axes=FALSE,
                     col=color)
      }
  }

if (is.null(trxControl)) {
  stop("You must designate a control treatment value via the trxControl parameter.  ")

} else {   # verify that there are control values in the table
  dfControl <- survData[survData[[trxName]] == trxControl,]
  if (length(dfControl[,1]) == 0) {
    stop(paste("No ", trxName, "values found to match ", trxControl))
  }
}

# implied "else" -- trxControl is not null and we have control values

pValTrx <- NULL
abbrevTrx <- abbreviate(trxVector,abbrevN)
if (!Ropen) {
  for (t in 1:length(trxVector)) {
    if (trxVector[t] != trxControl) {
      dfCurr <- survData[survData[[trxName]] == trxVector[t],]
      dfCurr <- rbind(dfControl, dfCurr)
      
# Create "Surv" object for analysis
      currSurvObj <- Surv(dfCurr[[timeName]], dfCurr[[statusName]])
      currsForm <- as.formula(paste("currSurvObj ~ ", trxName))

      stats <- survdiff(currsForm, data=dfCurr)
      pVal <- format(signif(1 - pchisq(stats$chisq,  1), 3))
        
      if (is.null(pValTrx) ) {
        pValTrx <- paste("p", abbrevTrx[trxVector==trxControl], ".", abbrevTrx[t], " = ", pVal, sep="")
      } else {
        pValTrx <- paste(pValTrx, paste("p", abbrevTrx[trxVector==trxControl], ".", abbrevTrx[t], " = ", pVal, sep=""))
      }
    }
  }  # end  for trxVector
}    # end if not open report

if(printPVals)
  mtext(pValTrx, side = 3, line=1, cex=pValCex)

# Y axis labels removing "0" as a label

yLabs <- c("",paste(100 * axTicks(2)[-1], sep=""))

axis(2, labels = yLabs, at = axTicks(2), las=2, cex.axis = cexAxis)  # "las=2" => horiz. label

axis(1, at=c(seq(from=0, to=msDays[length(msDays)], by=GraphSSfreq), msDays[length(msDays)]), tick=TRUE, cex.axis=cexAxis)

# Put nEvents by Treatment in legend for now
trxEvents <- survData[[statusName]][survData[[trxName]] == trxVector[1]]
if (Ropen)
  {
    nEvents <- sum(summary(fit)$n.event)
    legText <- c(paste(" nEvents = ", nEvents, sep=""))
  }
else {
  nEvents <- tapply(summary(fit)$n.event,summary(fit)$strata,sum)
  legText <- paste(trxVector, " nEvents = ", nEvents, sep="")
}

if(Legend)
  {
    if(LegendLoc=="bottomleft" | LegendLoc=="bottomright" | LegendLoc=="bottom")
      {
        if(printSS)
          {
            stop(warning("Legend Location cannot be in the bottom and printSS==TRUE"))
          }
      }
    legend(LegendLoc, legend=legText, lty = c(1:numTrx), cex=LegendCex, bty="n")
  }
# starting line for nRisk lines
if(!SSinPlot)
    abline(h=0)

tLine <- -3

if (printSS) {
for (t in 1:numTrx) {
  trxAll <- survData[survData[[trxName]] == trxVector[t],]  # all patients, current treatment
  
# Sample size for this trx
# also, prime the loop

  trxN <- length(trxAll[[trxName]][trxAll[[timeName]] >= msDays[1] ])
# trxAllN: array of sample sizes as of each milestone day
  trxAllN <- trxN
  for (i in 2:length(msDays)) {
    levelSurv <- length(trxAll[[trxName]][trxAll[[timeName]] >= msDays[i] ])
    trxAllN <- c(trxAllN, levelSurv)
    }

  if(uneven==FALSE)
    {
      mtext(trxAllN, side = 1, line =tLine,
            at=seq(from=0, to=msDays[length(msDays)], by=GraphSSfreq), cex=SScex)
    }
  else
    {
      mtext(trxAllN,side=1,line=tLine,
            at=c(seq(from=0, to=msDays[(length(msDays)-1)], by=GraphSSfreq),msDays[length(msDays)]),cex=SScex)
    }

  if (Ropen) {
    mtext(paste("nRisk", sep=" "),
          side = 1,
          line = tLine,
          at = 0 - (2 * xBase),
          adj=1,
          cex=SScex)
  } else {
    mtext(paste("nRisk", abbrevTrx[t], sep=" "),
          side = 1,
          line = tLine,
          at = 0 - (2 * xBase),
          adj=1,
          cex=SScex)
  }
  if(numTrx <= 3)
    {
      tLine <- tLine + 1
    }
  else if(numTrx > 3)
    {
      tLine <- tLine +0.5
    }
}
}   # end if printSS

box()
par(mar=defaultmar)
}