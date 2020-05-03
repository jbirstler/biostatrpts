rounds <- function(x,digits=0,as.numeric=FALSE)
  {
    ifelse(as.numeric,return(as.numeric(format(round(x,digits),nsmall=digits,trim=TRUE))),
           return(format(round(x,digits),nsmall=digits,trim=TRUE)))
  }
