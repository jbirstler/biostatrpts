#' Box Plot function embedded in other box plot functions
#' 
#' Function called by other box plot functions to actually create the box plot.
#' This allows for a consistent look throughout multiple different functions
#' 
#' This function is an adaptation of the traditional boxplot() so that box plot
#' functions in biostatrpts can be similar
#' 
#' uwBox is called by any biostatrpts function that creates box plots.
#' 
#' @param data data frame with metricName and trxName as columns
#' @param trxName column name of treatment factor in data
#' @param metricName column name of numeric variable in data
#' @param yLab (boxplot)(string) Y-axis label
#' @param xLab (boxplot)(string) X-axis label
#' @param yLim (boxplot)(numeric vector) Y-axis range
#' @param boxWex (boxplot)(numeric) see boxplot
#' @param plotMean (logical) if TRUE a dot will be plotted for where the mean
#' is
#' @param pOutliers (logical) if TRUE outliers are plotted
#' @return Returns a list of sample sizes (n), means (mean), standard
#' deviations (sd), minimums (min), first quartile (q25), medians (median),
#' third quartile (q75), maximums (max)
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @examples
#' 
#' 
#' TRT <- c(rep("A",20), rep("B",20), rep("C",20))
#' metric <- c(rnorm(20,2,1), rnorm(20,3,1), rnorm(20,4,1))
#' 
#' dat <- data.frame(TRT,metric)
#' 
#' uwBox(dat,"TRT","metric")
#' 
#' 
#' 
uwBox <- function(data,
                  trxName=NULL,
                  metricName,
                  yLab=NULL,
                  xLab=NULL,
                  yLim=NULL,
                  boxWex=0.75,
                  plotMean=TRUE,
                  pOutliers=TRUE)
  {

    if(is.null(trxName))
      bp <- boxplot(data[[metricName]],axes=FALSE,plot=FALSE)
    else
      bp <- boxplot(data[[metricName]] ~ data[[trxName]],axes=FALSE,plot=FALSE)

    #  Calculate values for box plot whiskers and box values
    #  Whisker calculated as largest observation <= 75th + 1.5xIQR or smallest observation >= 25th + 1.5xIQR
    #  Box calculated as 25th and 75th percentile
    if(is.null(trxName))
      {
        q25 <- quantile(data[[metricName]],probs=0.25,na.rm=TRUE)
        q75 <- quantile(data[[metricName]],probs=0.75,na.rm=TRUE)
        iqr <- IQR(data[[metricName]],na.rm=TRUE)
        mins <- min(data[[metricName]],na.rm=TRUE)
        maxs <- max(data[[metricName]],na.rm=TRUE)
        lWhisker <- ifelse(sum(data[[metricName]]<(q25-1.5*iqr))==0,mins,
                           ifelse(sort(data[[metricName]])[(sum(data[[metricName]]<(q25-1.5*iqr))+1)]>q25,q25,
                                  sort(data[[metricName]])[(sum(data[[metricName]]<(q25-1.5*iqr))+1)]))
        uWhisker <- ifelse(sum(data[[metricName]]>(q75+1.5*iqr))==0,maxs,
                           ifelse(sort(data[[metricName]],TRUE)[(sum(data[[metricName]]>(q75+1.5*iqr))+1)]<q75,q75,
                                  sort(data[[metricName]],TRUE)[(sum(data[[metricName]]>(q75+1.5*iqr))+1)]))
        q50 <- median(data[[metricName]],na.rm=TRUE)
        mn <- mean(data[[metricName]],na.rm=TRUE)
        sds <- sd(data[[metricName]],na.rm=TRUE)

        bp$stats[1] <- lWhisker
        bp$stats[2] <- q25
        bp$stats[3] <- q50
        bp$stats[4] <- q75
        bp$stats[5] <- uWhisker
      }
    else
      {
        q25 <- tapply(data[[metricName]],data[[trxName]],quantile,probs=0.25,na.rm=TRUE)
        q75 <- tapply(data[[metricName]],data[[trxName]],quantile,probs=0.75,na.rm=TRUE)
        iqr <- tapply(data[[metricName]],data[[trxName]],IQR,na.rm=TRUE)
        mins <- tapply(data[[metricName]],data[[trxName]],min,na.rm=TRUE)
        maxs <- tapply(data[[metricName]],data[[trxName]],max,na.rm=TRUE)
        lWhisker <- c()
        uWhisker <- c()
        for(i in 1:length(q25))
          {
            thisTrx <- na.omit(data[[metricName]][data[[trxName]]==levels(data[[trxName]])[i]])
            lWhisker[i] <- ifelse(sum(thisTrx<(q25[i]-1.5*iqr[i]))==0,mins[i],
                                  ifelse(sort(thisTrx)[(sum(thisTrx<(q25[i]-1.5*iqr[i]))+1)]>q25[i],q25[i],
                                         sort(thisTrx)[(sum(thisTrx<(q25[i]-1.5*iqr[i]))+1)]))
            uWhisker[i] <- ifelse(sum(thisTrx>(q75[i]+1.5*iqr[i]))==0,maxs[i],
                                  ifelse(sort(thisTrx,TRUE)[(sum(thisTrx>(q75[i]+1.5*iqr[i]))+1)]<q75[i],q75[i],
                                         sort(thisTrx,TRUE)[(sum(thisTrx>(q75[i]+1.5*iqr[i]))+1)]))
          }
        q50 <- tapply(data[[metricName]],data[[trxName]],median,na.rm=TRUE)
        mn <- tapply(data[[metricName]],data[[trxName]],mean,na.rm=TRUE)
        sds <- tapply(data[[metricName]],data[[trxName]],sd,na.rm=TRUE)

        bp$stats[1,] <- lWhisker
        bp$stats[2,] <- q25
        bp$stats[3,] <- q50
        bp$stats[4,] <- q75
        bp$stats[5,] <- uWhisker
      }

    if(is.null(yLim))
      yLim <- c(min(data[[metricName]],na.rm=TRUE),max(data[[metricName]],na.rm=TRUE))

# plot box plots
    bxp(bp,ylab=yLab,xlab=xLab,axes=FALSE,boxwex=boxWex,outline=FALSE,ylim=yLim)

    # add mean values if plotMean
    if(plotMean)
      points(x=1:length(bp$stats[1,]),y=mn,pch=16)

    # plot outliers if pOutliers
    if(pOutliers)
      {
        if(is.null(trxName))
          {
            if(length(data[[metricName]][data[[metricName]]<lWhisker | data[[metricName]]>uWhisker])>0)
              points(x=rep(1,length(data[[metricName]][data[[metricName]]<lWhisker | data[[metricName]]>uWhisker])),
                     y=data[[metricName]][data[[metricName]]<lWhisker | data[[metricName]]>uWhisker],pch=1)
          }
        else
          {
            for(i in 1:length(lWhisker))
              {
                thisTrx <- data[[metricName]][data[[trxName]]==levels(data[[trxName]])[i]]
                if(length(thisTrx[thisTrx<lWhisker[i] | thisTrx>uWhisker[i]])>0)
                  {
                    points(x=rep(i,length(thisTrx[thisTrx<lWhisker[i] | thisTrx>uWhisker[i]])),
                           y=thisTrx[thisTrx<lWhisker[i] | thisTrx>uWhisker[i]],pch=1,cex=0.6)
                  }
              }
          }
      }
    return(list("n"=bp$n,"mean"=mn,"sd"=sds,"min"=mins,"q25"=q25,"median"=q50,"q75"=q75,"max"=maxs))
  }
    
