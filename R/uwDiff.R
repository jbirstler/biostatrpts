

uwDiff <- function(data,varvec,trxName=NULL,VisitName,Baseline,ptID,deltaPct=FALSE,digits=1)
  {
    # Make sure data is a data frame
    if(!is.data.frame(data))
      stop("data isn't a data frame")
    # Make sure trxName is a column in data
    if(!is.null(trxName))
      if(!(trxName %in% colnames(data)))
        stop("trxName isn't a column in data")
    # Make sure varvec and VisitName are columns in data
    if(FALSE %in% (c(varvec,VisitName,ptID) %in% colnames(data)))
      stop("At least 1 varvec and/or VisitName and/or ptID isn't a column in data")
    # Make sure Baseline is the first level in VisitName
    if(!(levels(data[[VisitName]])[1]==Baseline))
      stop("Baseline needs to be the first level of VisitName.  Consider making VisitName ordered")

    
    DIFF <- function(x)
      {
        n <- length(x)
        x[2:n]-x[1]
      }
    PCT <- function(x)
      {
        n <- length(x)
        round(100*(x[2:n]-x[1])/x[1],digits)
      }
    for(i in 1:length(varvec))
       {
         thisData <- data.frame(var=data[[varvec[i]]],visit=data[[VisitName]],id=data[[ptID]])
         
         if(!is.null(trxName))
           {
             thisData$trx <- data[[trxName]]
             thisData$trx <- as.factor(thisData$trx)
           }
         
         ## Get Time levels
         VisitNameLevels <- levels(thisData$visit)
         ## Get Time:ID column for matching later
         thisData$visit.id <- as.factor(thisData$visit):as.factor(thisData$id)

         ## Check that each patient has a unique or no measure at each time point
         for(j in 1:length(VisitNameLevels))
           {
             vispts <- thisData$id[thisData$visit==VisitNameLevels[j]]
              if(length(vispts) != length(unique(vispts)))
               {
                 multi <- names(summary(as.factor(vispts)))[summary(as.factor(vispts)) > 1]
                 warning(paste("These patients",paste(multi,collapse=", "),"had more than 1 ",VisitNameLevels[j]," measure"))
               }
           }
         if(exists("multi"))
           stop("Program was stopped due to patients with multiple measures. See Warnings")

         ##  Need to make sure that everyone at baseline as values for each VisitName level even if that is NA
         fullData <- data.frame(visit.id=levels(as.factor(thisData$visit):as.factor(thisData$id)),
                                var=NA)
         fullData$visit <- as.factor(unlist(lapply(strsplit(as.character(fullData$visit.id),":",fixed=TRUE),"[[",1)))
         fullData$id <- as.factor(unlist(lapply(strsplit(as.character(fullData$visit.id),":",fixed=TRUE),"[[",2)))
         fullData$var <- thisData$var[match(fullData$visit.id,thisData$visit.id)]
         if(!is.null(trxName))
             fullData$trx <- thisData$trx[match(fullData$visit.id,thisData$visit.id)]

         ##  Take differences
         if(deltaPct)
           diff <- tapply(fullData$var,fullData$id,PCT)
         else
           diff <- tapply(fullData$var,fullData$id,DIFF)
         ##  Create data frame to be returned with all differences
         if(!exists("diffData"))
           {
             #  Remove baseline information
             diffData <- fullData[fullData$visit!=Baseline,]
             #  Order data frame to match order of differences
             diffData$visit <- ordered(diffData$visit,VisitNameLevels[-1])
             diffData <- diffData[order(diffData$id,diffData$visit),]
             #  Add differences
             diffData[[varvec[i]]] <- unlist(diff)
             #  Only include the data that was in the original data set, not added NAs
             diffData <- diffData[diffData$visit.id %in% thisData$visit.id,]
             ###  Do a little clean up
             diffData$visit.id <- NULL
             diffData$var <- NULL
             colnames(diffData)[colnames(diffData)=="id"] <- ptID
             colnames(diffData)[colnames(diffData)=="visit"] <- VisitName
             if(!is.null(trxName))
               colnames(diffData)[colnames(diffData)=="trx"] <- trxName
           }
         else
           diffData[[varvec[i]]] <- unlist(diff)
         
       }
    return(diffData)
  }
