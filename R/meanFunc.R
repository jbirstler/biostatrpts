meanFunc <- function(data,nvar,gvar=NULL,digits=1)
  {
    if(is.null(gvar))
      return(paste(rounds(mean(data[[nvar]],na.rm=TRUE),digits)," (",
                   rounds(sd(data[[nvar]],na.rm=TRUE),digits),")",sep=""))
    else
      return(paste(rounds(tapply(data[[nvar]],data[[gvar]],mean,na.rm=TRUE),digits)," (",
                   rounds(tapply(data[[nvar]],data[[gvar]],sd,na.rm=TRUE),digits),")",sep=""))
  }
