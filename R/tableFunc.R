tableFunc <- function(data,fvar,gvar=NULL,level=NULL,digits=1)
  {
    if(is.null(gvar))
      {
        if(is.null(level))
          return(paste(table(data[[fvar]])," (",
                       rounds(100*prop.table(table(data[[fvar]])),digits),"%)",sep=""))
        else
          return(paste(table(data[[fvar]])[level]," (",
                       rounds(100*prop.table(table(data[[fvar]]))[level],digits),"%)",sep=""))
      }
    else
      {
        if(is.null(level))
          return(paste(table(data[[fvar]],data[[gvar]])," (",
                       rounds(100*prop.table(table(data[[fvar]],data[[gvar]]),2),digits),"%)",sep=""))
        else
          return(paste(table(data[[fvar]],data[[gvar]])[level,]," (",
                       rounds(100*prop.table(table(data[[fvar]],data[[gvar]]),2)[level,],digits),"%)",sep=""))
      }
  }
