

figureBox <- function(boxPlot=TRUE,
                      demoBarPlot=FALSE,
                      pHoriz=pHoriz,
                      pTitle=pTitle,
                      titleCex=titleCex,
                      printPVals=printPVals,
                      pStrWrap=pStrWrap)
  {
    # figure out number of vertical units per line
    UperL <- ((par("usr")[4]-par("usr")[3])/par("pin")[2])/(par("mar")[3]/par("mai")[3])
    # figure out horizontal unites per line
    HperL <- ((par("usr")[2]-par("usr")[1])/par("pin")[1])/(par("mar")[3]/par("mai")[3])
    ###  Create box around sample sizes
    # Always on the bottom of the graphic unless bar plot with pHoriz=TRUE
    if(!boxPlot & pHoriz)
      rect(par("usr")[1]-0.8*HperL,par("usr")[3],par("usr")[1],par("usr")[4],xpd=TRUE)
    else
      rect(par("usr")[1],par("usr")[3]-0.8*UperL,par("usr")[2],par("usr")[3],xpd=TRUE)

    ###  Create box around title and P-values (if needed)
    if(printPVals)
      {
        #add box around p-values based on pStrWrap status
        if(!pStrWrap)
          {
             rect(par("usr")[1],par("usr")[4],par("usr")[2],par("usr")[4]+0.8*UperL,xpd=TRUE)
            if(!is.null(pTitle))
              {
                # put box around title
                 rect(par("usr")[1],par("usr")[4]+0.8*UperL,par("usr")[2],par("usr")[4]+2*UperL,xpd=TRUE)
                # print title
                title(main=pTitle,line=1.1,cex.main=titleCex,font.main=1)
              }
          }
        else
          {
            rect(par("usr")[1],par("usr")[4],par("usr")[2],par("usr")[4]+1.6*UperL,xpd=TRUE)
            if(!is.null(pTitle))
              {
                # put box around title
                rect(par("usr")[1],par("usr")[4]+1.6*UperL,par("usr")[2],par("usr")[4]+3*UperL,xpd=TRUE)
                 # print title
                title(main=pTitle,line=2.1,cex.main=titleCex,font.main=1)
              }
          }
      }
    else
      {
        if(!is.null(pTitle))
          {
            # put box around title
            rect(par("usr")[1],par("usr")[4],par("usr")[2],par("usr")[4]+1.2*UperL,xpd=TRUE)
                # print title
            title(main=pTitle,line=0.3,cex.main=titleCex,font.main=1)
          }
      }
    return(list(UperL,HperL))
  }
