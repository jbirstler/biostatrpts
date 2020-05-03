medianFunc <- function(data,nvar,gvar=NULL,digits=1,IQR=TRUE)
  {
    if(is.null(gvar))
      {
        if(IQR)
          return(paste(rounds(median(data[[nvar]],na.rm=TRUE),digits)," (",
                       rounds(quantile(data[[nvar]],probs=0.25,na.rm=TRUE),digits)," - ",
                       rounds(quantile(data[[nvar]],probs=0.75,na.rm=TRUE),digits),")",sep=""))
        else
          return(paste(rounds(median(data[[nvar]],na.rm=TRUE),digits)," (",
                       rounds(min(data[[nvar]],na.rm=TRUE),digits)," - ",
                       rounds(max(data[[nvar]],na.rm=TRUE),digits),")",sep=""))
      }
    else
      {
        if(IQR)
          return(paste(rounds(tapply(data[[nvar]],data[[gvar]],median,na.rm=TRUE),digits)," (",
                       rounds(tapply(data[[nvar]],data[[gvar]],quantile,probs=0.25,na.rm=TRUE),digits)," - ",
                       rounds(tapply(data[[nvar]],data[[gvar]],quantile,probs=0.75,na.rm=TRUE),digits),")",sep=""))
        else
          return(paste(rounds(tapply(data[[nvar]],data[[gvar]],median,na.rm=TRUE),digits)," (",
                       rounds(tapply(data[[nvar]],data[[gvar]],min,na.rm=TRUE),digits)," - ",
                       rounds(tapply(data[[nvar]],data[[gvar]],max,na.rm=TRUE),digits),")",sep=""))
      }
  }
