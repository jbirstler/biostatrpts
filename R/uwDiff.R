#' Function for finding differences between different factor levels for the
#' same patient for either numeric or ordinal variables
#' 
#' This function is meant for finding differences between different factor
#' levels for the same patient for either numeric or ordinal variables.  It is
#' utilized in uwLeveledBoxPlot.
#' 
#' 
#' @param data Name of data frame in R
#' @param varvec String or vector of variable name(s) of interest in data
#' @param trxName String of treatment variable name in data.  It is just copied
#' over to the new data frame
#' @param VisitName String of variable name for time points
#' @param Baseline String of level name in VisitName that is the baseline
#' value.  This must be the first level in VisitName too.
#' @param ptID String of variable name for Identifying unique subjects
#' @param deltaPct Logical. TRUE gives percentage change from baseline.  FALSE
#' gives differences from baseline
#' @param digits Numeric.  Number of decimals in the percentages if
#' deltaPct=TRUE
#' @return A data frame is returned with ID column, time column, treatment
#' column, and difference column(s).
#' 
#' Differences are set so the baseline value is being subtracted from the
#' non-baseline value.
#' 
#' Column names in difference data frame are the same names as that are given
#' in the arguments of uwDiff.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.  Assistance from Frontier Science
#' and Technology Reseach Foundation, Pat Lenon and Zekai Otles.
#' @seealso uwLeveledBoxPlot()
#' @examples
#' 
#' ## Example
#' 
#' ID <- rep(letters[1:20], 3)
#' TRT <- rep(c("A","B"), 30)
#' Time <- c(rep("Baseline",20),rep("Week 5",20),rep("Week 10",20))
#' Time <- ordered(Time, c("Baseline", "Week 5", "Week 10"))
#' AE <- sample(c(1,2,3,4),60,replace=TRUE)
#'   
#' data2 <- data.frame(ID,TRT,Time,AE)
#' 
#' uwDiff(data=data2,
#'        varvec="AE",
#'        trxName="TRT",
#'        VisitName="Time",
#'        Baseline="Baseline",
#'        ptID="ID",
#'        deltaPct=TRUE,
#'        digits=1)
#' 
#' 
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
