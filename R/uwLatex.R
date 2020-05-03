uwLatex <- function(mat,
                    file=NULL,
                    type=c("normal","multicol","landscape","longtable"),
                    transpose=FALSE,
                    twofactors=FALSE,
                    where="!htbp",
                    justify="center",
                    size="normalsize",
                    cgroup.type="bfseries",
                    cgroup=NULL,
                    n.cgroup=NULL,
                    cline=TRUE,
                    rowLines=FALSE,
                    cgroup.just="c",
                    col.just="c",
                    first.coltype="mdseries",
                    caption.loc="bottom",
                    caption=NULL,
                    label=NULL,
                    showTab=FALSE,
                    returnTab=FALSE)
  {
    #set defaults
    if(is.null(label))
      {label <- deparse(substitute(mat))}
    if(is.null(file))
      {file <- paste(getwd(),"/",label,".tex",sep="")}

    rgroup <- NULL
    
    for(i in 1:length(colnames(mat)))
      {
        colnames(mat)[i] <- gsub('<=', '$\\\\leq$', colnames(mat)[i])
        colnames(mat)[i] <- gsub('>=', '$\\\\geq$', colnames(mat)[i])
        colnames(mat)[i] <- gsub('<', '$<$', colnames(mat)[i])
        colnames(mat)[i] <- gsub('>', '$>$', colnames(mat)[i])
        colnames(mat)[i] <- gsub('%', '$\\\\%$', colnames(mat)[i])
      }
    #for(i in 1:length(mat[,1]))
    #  {
    #    mat[i,1] <- gsub('<=', '$\\\\leq$', mat[i,1])
    #    mat[i,1] <- gsub('>=', '$\\\\geq$', mat[i,1])
    #    mat[i,1] <- gsub('<', '$<$', mat[i,1])
    #    mat[i,1] <- gsub('>', '$>$', mat[i,1])
    #    mat[i,1] <- gsub('%', '$\\\\%$', mat[i,1])

    #    mat[i,ncol(mat)] <- gsub('< 0.001', '$<$ 0.001', mat[i,ncol(mat)])
    #  }
    mat <- gsub('<=', '$\\\\leq$', mat)
    mat <- gsub('>=', '$\\\\geq$', mat)
    mat <- gsub('<', '$<$', mat)
    mat <- gsub('>', '$>$', mat)
    mat <- gsub('%', '$\\\\%$', mat)
    mat <- gsub('< 0.001', '$<$ 0.001', mat)
 
    if(!is.null(cgroup))
      {
        for(i in 1:length(cgroup))
          {
            cgroup[i] <- gsub('<=', '$\\\\leq$', cgroup[i])
            cgroup[i] <- gsub('>=', '$\\\\geq$', cgroup[i])
            cgroup[i] <- gsub('<', '$<$', cgroup[i])
            cgroup[i] <- gsub('>', '$>$', cgroup[i])
            cgroup[i] <- gsub('%', '$\\\\%$', cgroup[i])
          }
      }

    if(transpose)
      {
        mat <- t(mat)
        mat <- cbind(rownames(mat), mat)
        rownames(mat) <- NULL
        if(twofactors)
          {
            # format first factor to be cgroup of transpose
            fact1 <- mat[1,]
            FactCount <- c()
            count <- 0
            for(i in 1:length(fact1)) # count "" to get n.cgroup
              {
                FactCount[i] <- 0
                if(fact1[i]=="")
                  {
                    fact1[i] <- NA
                    count <- count+1
                  }
                else
                  {
                    FactCount[i] <- 1
                    if(count!=0)
                      {
                        FactCount[(i-(count+1))] <- count+1
                        count <- 0
                      }
                  }
              }
            for(i in 1:length(FactCount))
              {
                if(FactCount[i]==0)
                  {FactCount[i] <- NA}
              }
            FactCount <- as.vector(na.omit(FactCount))
            FactCount[length(FactCount)] <- count+1
            fact1 <- as.vector(na.omit(fact1))
      
            # format second factor to be colnames of transpose
            fact2 <- mat[2,]
            #remove fact1 and fact2 from matrix
            mat <- mat[3:nrow(mat),]
            colnames(mat) <- fact2
            if(!is.null(cgroup))
              {
                rgroup <- cgroup[3:length(cgroup)]
                n.rgroup <- n.cgroup[3:length(n.cgroup)]

                fact1 <- c("",fact1)
                cgroup <- fact1
                n.cgroup <- c(1,FactCount)
              }  
          }
        else
          {
            fact1 <- mat[1,]
            mat <- mat[2:nrow(mat),]
            colnames(mat) <- fact1

            if(!is.null(cgroup))
              {
                rgroup <- cgroup[2:length(cgroup)]
                n.rgroup <- n.cgroup[2:length(n.cgroup)]

                cgroup <- NULL
                n.cgroup <- NULL
              }
          }
      }
      
    numCol <- ncol(mat)
    numRow <- nrow(mat)
    if(transpose && !is.null(rgroup))
      {
        numCol <- numCol+1
      }
    # Check and make sure rowLines is logical and the correct length
    if(!is.logical(rowLines))
      {
        warning("rowLines needs to be a logical vector, defaulted to FALSE")
        rowLines <- FALSE
      }
    if(length(rowLines)==1)
      {
        if(!rowLines)
          {rowLines <- rep(FALSE, nrow(mat))}
        else
          {rowLines <- rep(TRUE, nrow(mat))}
      }
    else
      {
        if(length(rowLines) > nrow(mat))
          {
            rowLines <- rowLines[1:nrow(mat)]
            warning("rowLines was too long and has been shortened")
          }
        else if(length(rowLines) < nrow(mat))
          {
            rowLines <- rep(rowLines, length=nrow(mat))
            warning("rowLines was too short and has been lengthed")
          }
      }
    if(length(cgroup.just)==1)
      {cgroup.just <- rep(cgroup.just[1], length(n.cgroup))}
    if(length(col.just)==1)
      {col.just <- rep(col.just[1], numCol)}

    #set commonly used strings to variable names
    sl <- paste("\\")
    begin <- paste(sl,"begin",sep="")
    hl <- paste(sl,"hline",sep="")
    capt <- paste(sl,"caption{",caption,sl,"label{",label,"}}",sep="")
    multcol <- paste(sl,"multicolumn",sep="")
    end <- paste(sl,"end",sep="")

    #check if mat is a matrix
    if(!is.matrix(mat)){stop("mat is not a matrix")}

    #check if cgroup is the same length as n.cgroup
    if(length(cgroup) != length(n.cgroup)){stop("length(cgroup) != length(n.cgroup)")}
    if(length(cgroup) != length(cgroup.just)){stop("length(cgroup) != length(cgroup.just)")}
    if(length(col.just) != numCol){stop("length(col.just) != ncol(mat)")}
    #get col.just in correct format for pasting
    col.just.start <- col.just
    #col.just.start[grep("|p",col.just.start,fixed=TRUE)] <- "|c"
    #col.just.start[grep("}|",col.just.start,fixed=TRUE)] <- "c|"
    #col.just.start[grep("p",col.just.start)] <- "c"
    coljust.f <- as.factor(col.just.start)
    coljust.len <- length(coljust.f)
    coljust <- NULL
    for(i in 1:coljust.len)
      {
        coljust <- paste(coljust.f[(coljust.len+1-i)],sep="",append=coljust)
      }                          
    #beginning of LaTeX table
    if(length(type) > 1)
      {
        type <- type[1]
      }
    if(type=="multicol")
      {
        placement <- paste(begin,"{tablehere}",sep="")
        tabtype <- "{tabular}"
      }
    else if(type=="landscape")
      {
        placement <- paste(begin,"{sidewaystable}","[",where,"]",sep="")
        tabtype <- "{tabular}"
      }
    else if(type=="longtable")
      {
        tabtype <- "{longtable}"
      }
    else
      {
        placement <- paste(begin,"{table}","[",where,"]",sep="")
        tabtype <- "{tabular}"
      }
    if(type=="longtable")
      {
        cat(c(paste(begin,"{",justify,"}",sep=""),paste("{",sl,size,sep=""),
              paste(begin,tabtype,"{",coljust,"}",hl,hl,sep=""),
              if(caption.loc=="top"){capt}), file=file,sep="\n")
      }
    else
      {
        cat(c(placement,paste(begin,"{",justify,"}",sep=""),
              if(caption.loc=="top"){capt},paste("{",sl,size,sep=""),
              paste(begin,tabtype,"{",coljust,"}",hl,hl,sep="")),
            file=file,sep="\n")
      }
    
    #body of LaTeX table based on transpose
    if(transpose)
      {
        mat <- cbind("",mat)
        numRow <- nrow(mat)
        numCol <- ncol(mat)
      }
    if(!is.null(cgroup))
      {
        if(length(cgroup) > 1)
          {
            for(i in 1:(length(cgroup)-1))
              {
                cat(paste(multcol,"{",n.cgroup[i],"}","{",cgroup.just[i],"}",
                          "{",sl,cgroup.type," ",cgroup[i],"}&",sep=""),
                    file=file,append=TRUE,sep="\n")
              }
          }
        # Last one, so it doesn't have & at the end
        cat(paste(multcol,"{",n.cgroup[length(cgroup)],"}","{",cgroup.just[length(cgroup)],"}",
                      "{",sl,cgroup.type," ",cgroup[length(cgroup)],"}",sep=""),file=file,
            append=TRUE,sep="\n")
        # Add cline{}
        if(cline)
          {
            cat(paste(sl,sl," ",sl,"cline{1-",sum(n.cgroup),"}",sep=""),
                file=file,append=TRUE,sep="\n")
          }
        else
          {
            cat(paste(sl,sl,sep=""),file=file,append=TRUE,sep="\n")
          }
      }
    for(j in 1:(numCol-1))
      {
        cat(paste(multcol,"{1}{",col.just[j],"}{",colnames(mat)[j],"}&",sep=""),file=file,
            append=TRUE,sep="\n")
      }
     # Last one, so it doesn't have & at the end
     cat(paste(multcol,"{1}{",col.just[numCol],"}{",colnames(mat)[numCol],"}",sep=""),
         file=file,append=TRUE,sep="\n")
     cat(paste(sl,sl," ",hl,sep=""),file=file,append=TRUE,sep="\n")

    # If longtable more needs to be added here.
    if(type=="longtable")
      {
        cat(paste(sl,"endfirsthead",sep=""),file=file,append=TRUE,sep="\n")
        cat(paste(hl,"  ",hl,sep=""),file=file,append=TRUE,sep="\n")
        #then redo what was done before in starting the body
        if(!is.null(cgroup))
          {
            if(length(cgroup) > 1)
              {
                for(i in 1:(length(cgroup)-1))
                  {
                    cat(paste(multcol,"{",n.cgroup[i],"}","{",cgroup.just[i],"}",
                              "{",sl,cgroup.type," ",cgroup[i],"}&",sep=""),
                        file=file,append=TRUE,sep="\n")
                  }
              }
                                        # Last one, so it doesn't have & at the end
            cat(paste(multcol,"{",n.cgroup[length(cgroup)],"}","{",cgroup.just[length(cgroup)],"}",
                      "{",sl,cgroup.type," ",cgroup[length(cgroup)],"}",sep=""),file=file,
                append=TRUE,sep="\n")
                                        # Add cline{}
            cat(paste(sl,sl," ",sl,"cline{1-",sum(n.cgroup),"}",sep=""),
                file=file,append=TRUE,sep="\n")
          }
        for(j in 1:(numCol-1))
          {
            cat(paste(multcol,"{1}{",col.just[j],"}{",colnames(mat)[j],"}&",sep=""),file=file,
                append=TRUE,sep="\n")
          }
                                        # Last one, so it doesn't have & at the end
        cat(paste(multcol,"{1}{",col.just[numCol],"}{",colnames(mat)[numCol],"}",sep=""),
            file=file,append=TRUE,sep="\n")
        cat(c(paste(sl,sl," ",hl,sep=""),paste(sl,"endhead",sep=""),paste(sl,sl," ",hl,sep=""),
              paste(multcol,"{",numCol,"}{r}{{Continued on next page}}",sep=""),
              paste(sl,sl," ",hl,sep=""),paste(sl,"endfoot",sep=""),paste(hl, " ",hl,sep=""),
              paste(sl,"endlastfoot",sep="")), file=file,append=TRUE,sep="\n")
      }
        
if(transpose && !is.null(rgroup))
      {
        if(!is.null(rgroup))
          {
            rowHeader <- rep("", sum(n.rgroup))
            rowHeadNum <- rep("", sum(n.rgroup))
            rowHeadPos <- c()
            for(i in 1:length(rgroup))
              {
                if(i == 1)
                  {
                    rowHeader[1] <- rgroup[1]
                    rowHeadNum[1] <- n.rgroup[1]
                    rowHeadPos[1] <- 1
                  }
                else
                  {
                    rowHeader[sum(n.rgroup[1:(i-1)])+1] <- rgroup[i]
                    rowHeadNum[sum(n.rgroup[1:(i-1)])+1] <- n.rgroup[i]
                    rowHeadPos[i] <- sum(n.rgroup[1:(i-1)])+1
                  }
              }
          }
      }

  # Add data from mat

    for(i in 1:numRow)
      {
        thisRow <- mat[i,]
        row.paste <- NULL
            for(j in 2:numCol)
              {
                row.paste<-paste("&",thisRow[(numCol+2-j)],sep="",
                                 append=row.paste)
              }

        if(!transpose | is.null(rgroup))
        {row.paste <- paste("{",sl,first.coltype," ",thisRow[1],"}",sep="",
                            append=row.paste)}
        else
          {
                if(i %in% rowHeadPos)
                  {
                    row.paste <- paste(sl,"multirow{",rowHeadNum[i],"}{*}{",rowHeader[i],"}",
                                       sep="",append=row.paste)
                  }
                else
                  {
                    row.paste <- paste("",append=row.paste)
                  }
              }
             
        if(!rowLines[i])
          {
            cat(paste(row.paste,sl,sl,sep=""),file=file,append=TRUE,sep="\n")
          }
        else
          {
            cat(paste(row.paste,sl,sl," ",hl,sep=""),file=file,append=TRUE,sep="\n")
          }
      }
  
    # End of LaTeX table

    if(type=="multicol")
      {
        endplace <- paste(end,"{tablehere}",sep="")
      }
    else if(type=="landscape")
      {
        endplace <- paste(end,"{sidewaystable}",sep="")
      }
    else if(type=="longtable")
      {
        endplace <- paste(end,"{longtable}",sep="")
      }
    else
      {
        endplace <- paste(end,"{table}",sep="")
      }

    if(type == "longtable")
      {
        cat(c(hl,if(caption.loc=="bottom"){capt},endplace,paste("}"),
              paste(end,"{",justify,"}",sep="")),
            file=file,append=TRUE,sep="\n")
      }
    else
      {
        cat(c(hl,paste(end,tabtype,sep=""),
              paste("}"),
              paste(end,"{",justify,"}",sep=""),
              if(caption.loc=="bottom"){capt},
              endplace),file=file,append=TRUE,sep="\n")
      }
    if(showTab)
      {
        structure(list(file=file), class='latex')
      }
    if(returnTab)
      {
        file.show(file)
      }
  
  }

