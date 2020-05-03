
compSweave <- function(filename, ...)
  {
    Sweave(paste(filename,'.Rnw',sep=""), stylepath=TRUE, ...)
    system(paste('latex',' ',filename,sep=""))
    system(paste('latex',' ',filename,sep=""))
    system(paste('dvips',' ',filename,sep=""))
  }
