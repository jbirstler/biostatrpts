inputTex <- function(texName,
                     clearpage=TRUE,
                     multicols=FALSE,
                     numCols=2)
  {
    #set commonly used strings to variable names
    sl <- paste("\\")
    clpage <- paste(sl,"clearpage",sep="")
    tx <- "tex"
    #set defaults
    
    start <- paste("\\input",sep='')
    
    #create code
   
    if(!multicols)
      {
        for(i in 1:length(texName))
          {
            cat(paste(start,"{",texName[i],".tex} ",sep=""),sep="\n")
          }
      }
    else
      {
        begin <- paste(sl,"begin{multicols}",sep="")
        cat(paste(begin,"{",numCols,"}",sep=""), sep="\n")

        for(i in 1:length(texName))
          {
           cat(paste(start,"{",texName[i],"}",sep=""),sep="\n")
         }
        cat(paste(sl,"end{multicols}",sep=""),sep="\n")
      }
    if(clearpage)
          {
            cat(clpage,sep="\n")
          }
  }

