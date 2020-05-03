#' Confidence Bands for Hazard Ratios, Mean Differences, or Odds Ratios
#' 
#' This function is meant the creation of 95\% confidence intervals for subsets
#' of a data set.  Confidence intervals can be for hazard ratios, mean
#' differences, or odds ratios.
#' 
#' Additional LaTeX Packages Required if LtxTranspose=TRUE:
#' usepackage{multirow} usepackage{longtable} if type="longtable" Special
#' functions Required: uwLatex()
#' 
#' @param fullData Name of the data frame loaded into R.
#' @param trxName String of name of treatment factor in fullData
#' @param trxLevels Vector of strings giving the two levels of trxName to be
#' used for the figure and table
#' @param factVarNames Vector of strings giving all of the variable names in
#' fullData that will be indivdually subsetted on.
#' @param factFigNames Vector of strings giving all of the names that will be
#' used in the figure.  Used when factVarNames are not actually how you want
#' the variables represented in the figure.  There is positional matching
#' between this and factVarNames
#' @param estimate String indicating which statistic should be estimated from
#' the data. "hazard", "mean", or "proportion" first letter is sufficient
#' @param pTitle String to be used as the Figure's title
#' @param coxMethod Method for coxph() in handling ties. Defaults to breslow"
#' @param timeName Only used if estimate="h".  String giving the variable name
#' that is gives the survival time
#' @param statusName Used if estimate="h" or estimate="p". String giving the
#' variable name that indicates whether or not the outcome of interest
#' occurred. This should be logical or 0's and 1's.
#' @param metricName Only used if estimate="m".  String giving the variable
#' name that the differences in means between treatment levels should be
#' calculated for.
#' @param printPVals (logical) TRUE to have p-values printed
#' @param interactPVals (logical) TRUE reports interaction p-values, FALSE is
#' regular treatment comparison p-values
#' @param xLim Specifies the limits of the x-axis.  If null, each method based
#' on estimate has it's own default
#' @param xLog Only used if estimate="h".  Logic for type of x-axis to be
#' printed. TRUE = log scale x-axis.  FALSE = unit scale x-axis.
#' @param xDetailed Only used if estimate="h".  Logic for type of x-axis
#' printed. TRUE has multiple default tick locations.  FALSE has default
#' locations based on axis(1)
#' @param leftMarWidth Sets the width of the left margin where the subset
#' indications will be printed
#' @param lineWidth Sets the width of the lines used for the confidence bands
#' @param pointSize The cex for the square box representing the point estimates
#' @param LatexFileName A vector of strings giving the folder and file(ending
#' in .tex) for the LaTeX table to be saved.  The default is NULL, which will
#' not save the table only print the matrix.
#' @param ... Any other arguement that can be passed to uwLatex()
#' @return Output is a figure and a LaTeX table. If LatexFileName is null, the
#' matrix of data is printed
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department and Frontier Science, Scott Hetzel M.S.
#' @seealso uwLatex()
#' @examples
#' 
#' 
#' data(colon) # from library(survival)
#' colon$sex.f <- factor(colon$sex,levels=c(0,1),labels=c("Female","Male"))
#' colon$surg.f <- factor(colon$surg, levels=c(0,1),
#' labels=c("Short","Long"))
#' colon$differ.f <- factor(colon$differ, levels=c(1,2,3),
#' labels=c("Well","Moderate","Poor"))
#' 
#' 
#' 
#' uwConfBands(fullDat=colon,
#'             trxName="rx",
#'             trxLevels=c("Obs","Lev"),
#'             factVarNames=c("sex.f","age","surg.f","differ.f"),
#'             factFigNames=c("Gender","Age","Time to Reg.","Tumor Diff."),
#'             estimate="m",
#'             metricName="nodes",
#'             printPVals=FALSE,
#'             LatexFileName=NULL)
#' 
#' 
#' uwConfBands(fullDat=colon,
#'             trxName="rx",
#'             trxLevels=c("Obs","Lev"),
#'             factVarNames=c("sex.f","age","surg.f","differ.f"),
#'             factFigNames=c("Gender","Age","Time to Reg.","Tumor Diff."),
#'             estimate="p",
#'             statusName="status",
#'             printPVals=TRUE,
#'             LatexFileName=NULL)
#' 
#' layout(matrix(c(1,1,1,1,2,2,2,2),nrow=2,byrow=TRUE))
#' uwConfBands(fullDat=colon,
#'             trxName="rx",
#'             trxLevels=c("Obs","Lev"),
#'             factVarNames=c("sex.f","age","surg.f","differ.f"),
#'             factFigNames=c("Gender","Age","Time to Reg.","Tumor Diff."),
#'             estimate="h",
#'             statusName="status",
#'             timeName="time",
#'             xLim=c(0.25,5),
#'             xLog=TRUE,
#'             printPVals=TRUE,
#'             interactPVals=TRUE,
#'             LatexFileName=NULL)
#' 
#' uwConfBands(fullDat=colon,
#'             trxName="rx",
#'             trxLevels=c("Obs","Lev"),
#'             factVarNames=c("sex.f","age","surg.f","differ.f"),
#'             factFigNames=c("Gender","Age","Time to Reg.","Tumor Diff."),
#'             estimate="h",
#'             statusName="status",
#'             timeName="time",
#'             xLim=c(0,2),
#'             xLog=FALSE,
#'             xDetailed=FALSE,
#'             printPVals=TRUE,
#'             interactPVals=TRUE,
#'             LatexFileName=NULL)
#' 
uwConfBands <- function(fullData,
                        trxName,
                        trxLevels,
                        factVarNames,
                        factFigNames=NULL,
                        estimate=c("hazard","mean","proportion"),
                        pTitle=NULL,
                        coxMethod=c("breslow","efron"),
                        timeName=NULL,
                        statusName=NULL,
                        metricName=NULL,
                        printPVals=TRUE,
                        interactPVals=FALSE,
                        xLim=NULL,
                        xLog=TRUE,
                        xDetailed=TRUE,
                        leftMarWidth=2,
                        lineWidth=6,
                        pointSize=1.3,
                        LatexFileName=NULL,
                        ...)
  {
    library(survival)
    library(gdata)
    # Set estimate type
    if(length(estimate) > 1)
      {
        estimate <- substr(estimate[1],1,1)
        warning("length(estimate) > 1, first entry is used")
      }
    else
        estimate <- substr(estimate,1,1)
      
    if(estimate != "h" & estimate != "m" & estimate != "p")
        stop("estimate is not an accepted selection out of c(hazard,mean,proportion)")
      
    # Set factVarNames for Figure
    if(!is.null(factFigNames))
      {
        if(length(factFigNames) != length(factVarNames))
            stop("length(factFigNames) != length(factVarNames)")
      }
    else
        factFigNames <- factVarNames
    
    #  Set trxLevels
    if(length(trxLevels) != 2)
        stop("length(trxLevels) != 2)")
      
    if(is.numeric(fullData[[trxName]]))
      {
        if(nlevels(as.factor(fullData[[trxName]])) != 2)
            stop("trxName is numeric with more than 2 outcomes.
                  Turn trxName into a factor then specify the two-way comparison with trxLevels")
          
        else
          {
            fullData[[trxName]] <- factor(fullData[[trxName]],
                                          levels=levels(as.factor(fullData[[trxName]])),
                                          labels=trxLevels, ordered=TRUE)
            warning("trxName is numeric and has been convertred to a factor with
                     trxLevels as factor levels.  trxLevels were assigned in numerical order.")
          }
      }
    if(sum(trxLevels %in% levels(fullData[[trxName]])) != 2)
        stop("At least one in trxLevels is not a level in fullData[[trxName]]")

    #  Set interactPVals to FALSE if printPVals is FALSE or if estimate is not for hazard ratio
    if(!printPVals | estimate != "h")
        interactPVals <- FALSE
      
    #  Remove unused Treatment levels and NA's in Treatment
    smData <- subset(fullData,
                         fullData[[trxName]]==trxLevels[1] | fullData[[trxName]]==trxLevels[2])
    smData <- smData[!is.na(smData[[trxName]]),]
    smData[[trxName]] <- ordered(smData[[trxName]],trxLevels)
    
    # Get number of bars for figure/ number of columns in table (+1 for All Randomized)
    check <- factVarNames %in% colnames(smData)
    if(sum(check) != length(factVarNames))
        stop("At least one of factVarNames is not a column in fullData")
      
    numBars <- c()
    barNames <- c()
    for(i in 1:length(factVarNames))
      {
        if(is.numeric(smData[[factVarNames[i]]]))
           {
             med <- round(median(smData[[factVarNames[i]]],na.rm=TRUE),1)
             smData[[factVarNames[i]]] <- factor(smData[[factVarNames[i]]] <= med,
                                                    levels=c(TRUE,FALSE),
                                                    labels=c(paste("<= ",med,sep=""),
                                                      paste("> ",med,sep="")),
                                                    ordered=TRUE)
           }
        smData[[factVarNames[i]]] <- drop.levels(smData[[factVarNames[i]]], reorder=FALSE)
        numBars <- sum(numBars,nlevels(smData[[factVarNames[i]]]))
        barNames <- c(barNames,paste(factFigNames[i],": ",levels(smData[[factVarNames[i]]]),sep=""))
      }
    numBars <- numBars+1

    # Create matrix for values to be used in the figure
    figMat <- matrix(nrow=numBars,ncol=3)
    rownames(figMat) <- c("All Randomized",barNames)
    colnames(figMat) <- c("PtEst","Low95","Up95")

    # Create matrix for values to be used in LaTeX table, remember rownames is 1st column
    if(!printPVals)
        texMat <- matrix("",nrow=numBars,ncol=9)
    else if(!interactPVals)
        texMat <- matrix("",nrow=numBars,ncol=10)
    else
        texMat <- matrix("",nrow=numBars,ncol=11)
      
    rownames <- NULL
    texMat[,1] <- c("All Randomized", barNames)
  
    if(estimate=="h")
      {
        #Set contrast options to "contr.treatment" for ordered variables to get correct estimates
        options(contrasts=c("contr.treatment","contr.treatment"))
        coxMethod <- coxMethod[1]
        if(coxMethod != "efron" & coxMethod != "breslow")
          {
            warning("coxMethod is not one of the valid possibilities. Defaulting to breslow")
            coxMethod <- "breslow"
           }
        if(is.null(statusName) | !(statusName %in% colnames(fullData)))
            stop("statusName must be supplied and a column in fullData for estimate=hazard")
          
        if(is.null(timeName) | !(timeName %in% colnames(fullData)))
            stop("timeName must be supplied and a column in fullData for estimate=hazard")

        subData <- subset(smData,select=c(trxName,statusName,timeName))
        # Remove any rows with NA in any column
        subData <- na.omit(subData)

        
        if(!printPVals)
            colnames(texMat) <- c("Subgroup",rep(c("Events/N","Pct"),3),"Ratio","(95% CI)")
        else if(!interactPVals)
            colnames(texMat) <- c("Subgroup",rep(c("Events/N","Pct"),3),"Ratio","(95% CI)","Value")
        else
            colnames(texMat) <- c("Subgroup",rep(c("Events/N","Pct"),3),"Ratio",
                                  "(95% CI)","Main","Interact")
        
        # Fit Cox Proportional Hazards Model
        surv <- Surv(time=subData[[timeName]], subData[[statusName]])
        cox <- coxph(surv ~ subData[[trxName]], method=coxMethod)
       
        # Get Hazard Ratio, Lower 95, and Upper 95
        figMat[1,] <- round(c(summary(cox)$conf[1],summary(cox)$conf[3:4]),2)
        texMat[1,8] <- figMat[1,1]
        texMat[1,9] <- paste("(",figMat[1,2],",",figMat[1,3],")",sep="")
        if(printPVals)
          {
            texMat[1,10] <- ifelse(summary(cox)$coef[4] < 0,
                                   round(2*pnorm(summary(cox)$coef[4]),3),
                                   round(2*pnorm(summary(cox)$coef[4],lower.tail=FALSE),3))
            texMat[1,10] <- ifelse(texMat[1,10]==0,"< 0.001",texMat[1,10])
          }
        
        # Get Table Fractions
        tab <- table(factor(subData[[statusName]],levels=c(1,0),labels=c(1,0),ordered=TRUE),
                     subData[[trxName]])

        texMat[1,2:7] <- c(paste(sum(tab[1,]),"/",sum(tab),sep=""),
                           round(100*(sum(tab[1,])/sum(tab)),1),
                           paste(tab[1,1],"/",sum(tab[,1]),sep=""),
                           round(100*(tab[1,1]/sum(tab[,1])),1),
                           paste(tab[1,2],"/",sum(tab[,2]),sep=""),
                           round(100*(tab[1,2]/sum(tab[,2])),1))
        k <- 2
        for(i in 1:length(factVarNames))
          {
            
            for(j in 1:nlevels(smData[[factVarNames[i]]]))
              {
                thisSub <- subset(smData,
                                  smData[[factVarNames[i]]]==levels(smData[[factVarNames[i]]])[j],
                                  select=c(trxName,statusName,timeName,factVarNames[i]))               
                thisSub <- na.omit(thisSub)
                thisSub[[trxName]] <- drop.levels(thisSub[[trxName]], reorder=FALSE)
                
                # Make sure treatment has two levels with patients in both
                if(length(summary(thisSub[[trxName]]))==2)
                  {
                    # Fit Cox Proportional Hazards Model
                    surv <- Surv(time=thisSub[[timeName]], thisSub[[statusName]])
                    cox <- coxph(surv ~ thisSub[[trxName]], method=coxMethod)

                    # Get Hazard Ratio, Lower 95, and Upper 95
                    figMat[k,] <- round(c(summary(cox)$conf[1],summary(cox)$conf[3:4]),2)
                    texMat[k,8] <- figMat[k,1]
                    texMat[k,9] <- paste("(",figMat[k,2],",",figMat[k,3],")",sep="")
                    
                    if(printPVals)
                      {
                        texMat[k,10] <- ifelse(summary(cox)$coef[4] < 0,
                                               round(2*pnorm(summary(cox)$coef[4]),3),
                                               round(2*pnorm(summary(cox)$coef[4],lower.tail=FALSE),3))
                        texMat[k,10] <- ifelse(texMat[k,10]==0,"< 0.001",texMat[k,10])
                      }
        
                    # Get Table Fractions
                    tab <- table(factor(thisSub[[statusName]],levels=c(1,0),labels=c(1,0),ordered=TRUE),
                                 thisSub[[trxName]])
                
                    texMat[k,2:7] <- c(paste(sum(tab[1,]),"/",sum(tab),sep=""),
                                       round(100*(sum(tab[1,])/sum(tab)),1),
                                       paste(tab[1,1],"/",sum(tab[,1]),sep=""),
                                       round(100*(tab[1,1]/sum(tab[,1])),1),
                                       paste(tab[1,2],"/",sum(tab[,2]),sep=""),
                                       round(100*(tab[1,2]/sum(tab[,2])),1))
                  
                    if(tab[1,1]==0 | tab[1,2]==0)
                      {
                        texMat[k,8:9] <- c("NA","NA")
                        if(printPVals)
                          {
                            texMat[k,10] <- "NA"
                          }
                        figMat[k,] <- rep(NA,3)
                      }
                   
                  }
                else
                  {
                    figMat[k,] <- rep(NA,3)
                    tab <- table(factor(thisSub[[statusName]],levels=c(1,0),
                                        labels=c(1,0),ordered=TRUE),thisSub[[trxName]])
                    
                    texMat[k,2:3] <- c(paste(tab[1,1],"/",sum(tab),sep=""),
                                       round(100*tab[1,1]/sum(tab),1))
                    if(colnames(tab)==trxLevels[1])
                      {
                        texMat[k,4:5] <- texMat[k,2:3]
                        texMat[k,6:9] <- rep("NA",3)
                        if(printPVals)
                          {
                            texMat[k,10] <- "NA"
                          }
                      }
                    else
                      {
                        texMat[k,4:5] <- c("NA","NA")
                        texMat[k,6:7] <- texMat[k,2:3]
                        texMat[k,8:9] <- c("NA","NA")
                        if(printPVals)
                          {
                            texMat[k,10] <- "NA"
                          }
                      }
                  }
                        
                k <- k+1
              }
            if(interactPVals)
              {
                thisFact <- subset(smData, select=c(trxName,factVarNames[i],
                                             timeName,statusName))
                thisFact <- na.omit(thisFact)
                thisFact[[trxName]] <- drop.levels(thisFact[[trxName]],reorder=FALSE)

                if(length(summary(thisFact[[trxName]]))==2)
                  {
                    #Fit Cox Proportional Hazards Model with Interaction
                    surv <- Surv(time=thisFact[[timeName]], thisFact[[statusName]])
                    cox <- coxph(surv ~ thisFact[[trxName]]*thisFact[[factVarNames[i]]])
                    texMat[(k-1),11] <- ifelse(round(anova(cox,test="Chisq")$P[4],3)==0,
                                          "< 0.001",round(anova(cox,test="Chisq")$P[4],3))
                  }
                else
                  {
                    texMat[(k-1),11] <- "NA"
                  }
              }     
          }
        # Reset options$contrast to default
        options(contrast=c("contr.treatment","contr.poly"))
      }
    
    else if(estimate=="m")
      {
        if(is.null(metricName) | !(metricName %in% colnames(fullData)))
          {
            stop("metricName must be supplied and a column in fullData for estimate=mean")
          }

        subData <- subset(smData, select=c(trxName, metricName))
        # Remove any rows with NA in any columns
        subData <- na.omit(subData)

        if(!printPVals)
            colnames(texMat) <- c("Subgroup",rep(c("N","Mean"),3),"Diff.","(95% CI)")
        else
            colnames(texMat) <- c("Subgroup",rep(c("N","Mean"),3),"Diff.","(95% CI)","Value")
          
        # Run t.test for everyone
        ttest <- t.test(subData[[metricName]])
        texMat[1,2] <- ttest$parameter+1
        texMat[1,3] <- round(ttest$estimate,2)

        # Run t.test by treatments
        ttest <- t.test(subData[[metricName]]~subData[[trxName]])
        figMat[1,] <- c(ttest$estimate[1]-ttest$estimate[2],ttest$conf[1],ttest$conf[2])
        texMat[1,4:8] <- round(c(summary(subData[[trxName]])[1],
                                 ttest$estimate[1],
                                 summary(subData[[trxName]])[2],
                                 ttest$estimate[2],
                                 ttest$estimate[1]-ttest$estimate[2]),2)
        texMat[1,9] <- paste("(",round(ttest$conf[1],2),",",round(ttest$conf[2],2),")",sep="")
        
        if(printPVals)
            texMat[1,10] <- ifelse(round(ttest$p.value,3)==0,"< 0.001",round(ttest$p.value,3))
        
        k <- 2
        for(i in 1:length(factVarNames))
          {
            for(j in 1:nlevels(smData[[factVarNames[i]]]))
              {
                thisSub <- subset(smData,
                                  smData[[factVarNames[i]]]==levels(smData[[factVarNames[i]]])[j],
                                  select=c(trxName,metricName,factVarNames[i]))
                thisSub <- na.omit(thisSub)
                # Run t.test for everyone
                ttest <- t.test(thisSub[[metricName]])
                texMat[k,2] <- ttest$parameter+1
                texMat[k,3] <- round(ttest$estimate,2)

                # Run t.test by treatments
                ttest <- t.test(thisSub[[metricName]]~thisSub[[trxName]])
                figMat[k,] <- c(ttest$estimate[1]-ttest$estimate[2],ttest$conf[1],ttest$conf[2])
                texMat[k,4:8] <- round(c(summary(thisSub[[trxName]])[1],
                                         ttest$estimate[1],
                                         summary(thisSub[[trxName]])[2],
                                         ttest$estimate[2],
                                         ttest$estimate[1]-ttest$estimate[2]),2)
                texMat[k,9] <- paste("(",round(ttest$conf[1],2),",",round(ttest$conf[2],2),")",sep="")
                if(printPVals)
                    texMat[k,10] <- ifelse(round(ttest$p.value,3)==0,"< 0.001",round(ttest$p.value,3))
                  
                k <- k+1
              }
          }
      }
    else
      {
        if(is.null(statusName) | !(statusName %in% colnames(fullData)))
            stop("statusName must be supplied and a column in fullData for estimate=proportion")

        subData <- subset(smData, select=c(trxName, statusName))
        # Remove any rows with NA in any columns
        subData <- na.omit(subData)

        # Check class of statusName and adjust if necessary
        if(is.numeric(subData[[statusName]]))
          {
            nums <- sort(unique(subData[[statusName]]))
            if(length(nums) != 2 | nums[1] != 0 | nums[2] != 1)
                stop("if statusName is numeric it must be only 0's and 1's. 1 indicating TRUE event")
          }
        else if(!is.logical(subData[[statusName]]))
            stop("statusName must be numeric or logical")

        if(printPVals)
            colnames(texMat) <- c("Subgroup",rep(c("Events/N","Pct"),3),"Ratio","(95% CI)","Value")
        else
            colnames(texMat) <- c("Subgroup",rep(c("Events/N","Pct"),3),"Ratio","(95% CI)")

        # Get Table Fractions
        tab <- table(factor(subData[[statusName]],levels=c(1,0),labels=c(1,0),ordered=TRUE),
                     subData[[trxName]])

        texMat[1,2:7] <- c(paste(sum(tab[1,]),"/",sum(tab),sep=""),
                           round(100*(sum(tab[1,])/sum(tab)),1),
                           paste(tab[1,1],"/",sum(tab[,1]),sep=""),
                           round(100*(tab[1,1]/sum(tab[,1])),1),
                           paste(tab[1,2],"/",sum(tab[,2]),sep=""),
                           round(100*(tab[1,2]/sum(tab[,2])),1))
        # Get Odds Ratio and Fisher P-Value
        fish <- fisher.test(tab)

        texMat[1,8:9] <- c(round(fish$estimate,2),
                            paste("(",round(fish$conf[1],2),",",round(fish$conf[2],2),")",sep=""))
        if(printPVals)
           texMat[1,10] <- ifelse(round(fish$p.v,3)==0,"< 0.001",round(fish$p.v,3))

        figMat[1,] <- c(fish$estimate,fish$conf)

        k <- 2
        for(i in 1:length(factVarNames))
          {
            for(j in 1:nlevels(smData[[factVarNames[i]]]))
              {
                thisSub <- subset(smData,
                                  smData[[factVarNames[i]]]==levels(smData[[factVarNames[i]]])[j],
                                  select=c(trxName,statusName,factVarNames[i]))
                thisSub <- na.omit(thisSub)
                # Get Table Fractions
                tab <- table(factor(thisSub[[statusName]],levels=c(1,0),labels=c(1,0),ordered=TRUE),
                             thisSub[[trxName]])

                texMat[k,2:7] <- c(paste(sum(tab[1,]),"/",sum(tab),sep=""),
                                   round(100*(sum(tab[1,])/sum(tab)),1),
                                   paste(tab[1,1],"/",sum(tab[,1]),sep=""),
                                   round(100*(tab[1,1]/sum(tab[,1])),1),
                                   paste(tab[1,2],"/",sum(tab[,2]),sep=""),
                                   round(100*(tab[1,2]/sum(tab[,2])),1))
                 # Get Odds Ratio and Fisher P-Value
                fish <- fisher.test(tab)

                texMat[k,8:9] <- c(round(fish$estimate,2),
                                    paste("(",round(fish$con[1],2),",",round(fish$con[2],2),")",sep=""))
                if(printPVals)
                    texMat[k,10] <- ifelse(round(fish$p.v,3)==0,"< 0.001",round(fish$p.v,3))

                figMat[k,] <- c(fish$estimate,fish$conf)
                k <- k+1
              }
          }
      }
    
# Create Figure
    defaultMargs <- par("mai")
    newMargs <- c(defaultMargs[1],leftMarWidth,defaultMargs[3:4])
    par(mai=newMargs)
    bandLoc <- c(0:(nrow(figMat)-1))+0.5
    if(estimate=="h")
      {
        if(is.null(xLim))
            xLim <- c(ifelse(min(figMat,na.rm=TRUE) >= 1, 0.75,min(figMat,na.rm=TRUE)),
                      ifelse(max(figMat,na.rm=TRUE) <= 1, 1.25,max(figMat,na.rm=TRUE)))
        else
          {
            if(xLog)
              if(sum(xLim <= 0) > 0)
                stop("xLim must be greater than 0 for logarithmic scale of estimate=h")
          }
        if(xLog)
          plot(x=exp(1),y=0,type="n",ylab="",xlab="Hazard Ratio", yaxt="n",xaxt="n",
               ylim=c(0,nrow(figMat)), xlim=xLim,main=pTitle, log="x")
        else
          plot(x=1,y=0,type="n",ylab="",xlab="Hazard Ratio", yaxt="n",xaxt="n",
               ylim=c(0,nrow(figMat)), xlim=xLim,main=pTitle)
        abline(v=1)
        if(xDetailed)
          axis(1,at=c(seq(from=0.1,0.5,by=0.1),0.75,1,1.25,1.5,1.75,seq(from=2,to=50,by=0.5)))
        else
          axis(1)
        mtext("95% Confidence Intervals for Hazard Ratio",side=3)
        if(!printPVals)
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Hazard","")
        else if(!interactPVals)
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Hazard","","P")
        else
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Hazard","","P-Values")
      }
    else if(estimate=="m")
      {
        if(is.null(xLim))
          {
            mx <- ceiling(max(abs(figMat)))
            xLim <- c(-mx,mx)
          }
        plot(x=0,y=0,type="n",ylab="",xlab="Mean Difference", yaxt="n",
             ylim=c(0,nrow(figMat)), xlim=xLim,main=pTitle)
        abline(v=0)
        mtext("95% Confidence Intervals for Average Difference",side=3)
        if(printPVals)
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Mean","","P")
        else
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Mean","")
      }
    else if(estimate=="p")
      {
        if(is.null(xLim))
            xLim <- c(ifelse(min(figMat,na.rm=TRUE) >= 1, 0.75,min(figMat,na.rm=TRUE)),
                      ifelse(max(figMat,na.rm=TRUE) <= 1, 1.25,max(figMat,na.rm=TRUE)))
        else
          {
            if(sum(xLim <= 0) > 0)
              stop("xLim must be greater than 0 for logarithmic scale of estimate=p")
          }
           
        plot(x=exp(1),y=0,type="n",ylab="",xlab="Odds Ratio", yaxt="n", xaxt="n",
             ylim=c(0,nrow(figMat)), xlim=xLim,main=pTitle, log="x")
        axis(1,at=c(seq(from=0.1,0.5,by=0.1),0.75,1,1.25,1.5,1.75,seq(from=2,to=50,by=0.5)))
        abline(v=1)
        mtext("95% Confidence Intervals for Odds Ratio",side=3)
        if(printPVals)
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Odds","","P")
        else
            cgroup <- c("","Total",trxLevels[1],trxLevels[2],"Odds","")
      }
    axis(2,bandLoc,rev(rownames(figMat)),tck=0,las=1)
    for(i in 1:nrow(figMat))
      {
        j <- nrow(figMat)-(i-1)
        lines(y=rep(bandLoc[i],2), x=c(figMat[j,2],figMat[j,3]),
              col="gray50", lend="round",lwd=lineWidth)
        points(y=bandLoc[i], x=figMat[j,1], pch=15, cex=pointSize)
      }
    # reset graphical parameters
    par(mai=defaultMargs)

    # Latex Table
    if(!printPVals)
      {
        n.cgroup <- c(1,2,2,2,1,1)
        cgroup.just <- c("|c|","c|","c|","c|","|c","c|")
        col.just <- c("|l|","c","c|","c","c|","c","c|","c","c|")
      }
    else if(!interactPVals)
      {
        n.cgroup <- c(1,2,2,2,1,1,1)
        cgroup.just <- c("|c|","c|","c|","c|","|c","c","|c|")
        col.just <- c("|l|","c","c|","c","c|","c","c|","c","c","|c|")
      }
    else
      {
        n.cgroup <- c(1,2,2,2,1,1,2)
        cgroup.just <- c("|c|","c|","c|","c|","|c","c","|c|")
        col.just <- c("|l|","c","c|","c","c|","c","c|","c","c","|c","c|")
      }
    if(is.null(LatexFileName))
      {
        print(texMat)
        warning("LatexFileName is null.  Latex table was not saved")
      }
    else
      {
        uwLatex(texMat, file=LatexFileName, cgroup=cgroup, n.cgroup=n.cgroup,
                cgroup.just=cgroup.just, cline=FALSE,
                col.just=col.just, ...)
      }
  }

        

    
