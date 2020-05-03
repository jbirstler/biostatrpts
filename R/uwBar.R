

uwBar <- function(table,
                  pBeside=TRUE,
                  pHoriz=FALSE,
                  lined=FALSE,
                  axLim=NULL,
                  yLab=NULL,
                  xLab=NULL,
                  printBarVals=TRUE,
                  Legend=FALSE,
                  LegendLoc="topright",
                  LegendCex=0.8,
                  mgp.y=c(3,1,0),
                  mgp.x=c(3,1,0),
                  cex.names=1,
                  barNamesLas=1,
                  barNamesSeq=1,
                  barNamesAngle=0,
                  digits=1)

  {
    # Need to switch order of the table around if pHoriz=TRUE
    if(pHoriz)
      {
        if(length(dim(table)) == 1)
          {
            table <- rev(table)
          }
        else
          {
            tempcol <- colnames(table)
            temprow <- rownames(table)
            table <- matrix(rev(table),nrow=nrow(table), ncol=ncol(table))
            colnames(table) <- rev(tempcol)
            rownames(table) <- rev(temprow)
          }
      }
    if(!pBeside)
      {
        if(length(dim(table))==1)
          {
            table <- matrix(sum(table))
            dim(table) <- 1
          }
        else
          table(apply(table,2,rev))
      }

    # If pBeside=FALSE then printBarVals has to be FALSE
    if(!pBeside & printBarVals==TRUE)
      warning("If pBeside=FALSE then printBarVals has to be FALSE")
    printBarVals <- ifelse(pBeside,printBarVals,FALSE)

    # Get Bar Colors based on number of rows in table
    if(length(dim(table)) == 1)
      barCol <- "gray50"
    else
      {
        barNum <- ceiling(seq(from=10, to=90, length=nrow(table)))
        barCol <- paste("gray",barNum,sep="")
      }
    
    angles <- NULL
    dens <- NULL
    if(lined)
      {
        angles <- rep(c(0,45,0,135),length=length(barCol))
        dens1 <- rev(ceiling(seq(from=8, to=40,
                                   length=floor(length(barCol)/2))))
        dens <- rep(150,length(barCol))
        dens[seq(1,length(barCol),1) %% 2 == 0] <- dens1
      }
    if(Legend)
      {
        legCol <- rev(barCol)
        legAng <- rev(angles)
        legDens <- rev(dens)
        if(pHoriz)
          {
            legText <- rev(rownames(table))
          }
        else
          {
            if(pBeside)
              legText <- rownames(table)
            else
              legText <- rev(rownames(table))
          }
      }
    if(!pHoriz & pBeside)
      {
        barCol <- rev(barCol)
        angles <- rev(angles)
        dens <- rev(dens)
      }
    if(is.null(axLim))
      {
        if(pBeside)
          axLim <- c(-0.02*ceiling((max(table,na.rm=TRUE)+(max(table,na.rm=TRUE)*0.1))),
                     ceiling((max(table,na.rm=TRUE)+(max(table,na.rm=TRUE)*0.1))))
        else
          {
           if(length(dim(table))==1)
             axLim <- c(-0.02*ceiling((max(table,na.rm=TRUE))+
                                       (max(table,na.rm=TRUE))*0.1),
                        ceiling((max(table,na.rm=TRUE))+
                                 (max(table,na.rm=TRUE))*0.1))
           else
             axLim <- c(-0.02*ceiling((max(apply(table,2,sum,na.rm=TRUE))+
                                       (max(apply(table,2,sum,na.rm=TRUE))*0.1))),
                        ceiling((max(apply(table,2,sum,na.rm=TRUE))+
                                 (max(apply(table,2,sum,na.rm=TRUE))*0.1))))
          }
      }
    else if(axLim[1]==0)
      {
        axLim <- c(-0.02*ceiling(axLim[2]*1.1),axLim[2])
      }
    if(pHoriz)
      {
        xlim <- axLim
        ylim <- NULL
      }
    else
      {
        xlim <- NULL
        ylim <- axLim
      }
    
    # Temp keep dimension names to delete them from bring printed
    dimNames <- dimnames(table)
    dimnames(table) <- NULL

    if(pBeside)
      space <- c(0.2,0.8)
    else
      space <- NULL

    bp <- barplot(table, 
                  beside=pBeside,
                  horiz=pHoriz,
                  main="",
                  space=space,
                  col=barCol,
                  angle=angles,
                  density=dens,
                  xlab="",
                  ylab="",
                  xlim=xlim,
                  ylim=ylim,
                  axes=FALSE)
    box()
    # Restore dimension names to use them later
    dimnames(table) <- dimNames

    # Create dummy table for easy of labeling in graph
    labTab <- table
   
    if(!pBeside & length(dimnames(table))==2)
      {
        labTab <- apply(labTab,2,sum)
      }
    
    # Add Legend
    if(Legend)
      {
        legend(LegendLoc, legend=legText, fill=legCol, cex=LegendCex,
               angle=legAng, density=legDens)
      }

    # Add Axes
    if(pHoriz)
      {
        axs <- axis(side=1,tcl=0,mgp=mgp.x,cex.axis=cex.names,las=barNamesLas)
        if(length(dimnames(labTab))==2)
          {
            if(barNamesAngle==0)
              axis(side=2,tcl=0,mgp=mgp.y,cex.axis=cex.names,las=barNamesLas,
                   at=apply(bp,2,mean)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   labels=colnames(labTab)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)])
            else
              text(x=par("usr")[1], y=apply(bp,2,mean)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   labels=colnames(labTab)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   srt=barNamesAngle,xpd=TRUE,pos=2,offset=1,cex=cex.names)
          }
        else
          {
            if(barNamesAngle==0)
              axis(side=2,tcl=0,mgp=mgp.y,cex.axis=cex.names,las=barNamesLas,
                   at=bp[seq(1,length(bp),by=barNamesSeq)],
                   labels=names(labTab)[seq(1,length(bp),by=barNamesSeq)])
            else
              text(x=par("usr")[1], y=bp[seq(1,length(bp),by=barNamesSeq)],
                   labels=names(labTab)[seq(1,length(bp),by=barNamesSeq)],
                   srt=barNamesAngle,xpd=TRUE,pos=2,offset=1,cex=cex.names)
          }
      }
    else
      {
        axs <- axis(side=2,tcl=-0.2,mgp=mgp.y,cex.axis=cex.names,las=barNamesLas)
        if(length(dimnames(labTab))==2)
          {
            if(barNamesAngle==0)
              axis(side=1,tcl=0,mgp=mgp.x,cex.axis=cex.names,las=barNamesLas,
                   at=apply(bp,2,mean)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   labels=colnames(labTab)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)])
            else
              text(y=par("usr")[3], x=apply(bp,2,mean)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   labels=colnames(labTab)[seq(1,length(apply(bp,2,mean)),by=barNamesSeq)],
                   srt=barNamesAngle,xpd=TRUE,pos=1,offset=2.5,cex=cex.names)
          }
        else
          {
            if(barNamesAngle==0)
              axis(side=1,tcl=0,mgp=mgp.x,cex.axis=cex.names,las=barNamesLas,
                   at=bp[seq(1,length(bp),by=barNamesSeq)],
                   labels=names(labTab)[seq(1,length(bp),by=barNamesSeq)])
            else
              text(y=par("usr")[3],x=bp[seq(1,length(bp),by=barNamesSeq)],
                   labels=names(labTab)[seq(1,length(bp),by=barNamesSeq)],
                   srt=barNamesAngle,xpd=TRUE,pos=1,offset=2.5,cex=cex.names)
          }
      }
    # Add Axis Labels
    if(!is.null(yLab))
      title(ylab=yLab,line=mgp.y[1])
    if(!is.null(xLab))
      title(xlab=xLab,line=mgp.x[1])
    
    # Add Bar Values
    if(printBarVals)
      {
        if(pHoriz)
          text(x=(table+(0.05*max(axs))),y=bp,labels=rounds(table,digits=digits),cex=0.6)
        else
          text(x=bp,y=(table+(0.05*max(axs))),labels=rounds(table,digits=digits),cex=0.6)
      }
    
    return(list(bp,table,axs))
  }


    
