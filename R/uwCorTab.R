#' Correlation Tables
#' 
#' This function is meant for creating correlation tables of one variable with
#' multiple other variables
#' 
#' 
#' @param data The data frame that all the variables are found
#' @param yvar String of the name in the data frame for the "response" variable
#' @param xvec Vector of strings giving the names in the data frame for the
#' other variables to be correlated with yvar
#' @param factor Vector for a two-level variable that can be used to seperate
#' the data and have a test for differences in correlations based on Fisher's
#' Z-transformation
#' @param colNames Manually put in the names for the columns of the table
#' produced. If factor is null and printPVals is FALSE ncol=3.  If factor is
#' null and printPVals is TRUE ncol=4.  If factor is not null and printPVals is
#' FALSE ncol=7 (printPVals TRUE, ncol=8).
#' @param printPVals Logical: Is forced to FALSE if no factor is provided
#' @param method Method to be used in the call of the cor() function
#' @param LatexFileName File name for the LaTeX table that is created using
#' uwLatex()
#' @param returnMat Logical: Do you want the matrix to be printed in R
#' @param digits Number of decimals to be printed
#' @param ... Other arguments that can be passed to uwLatex().  cgroup and
#' n.cgroup is already set
#' @return Output is the matrix that is created and used by uwLatex().  Code
#' for the LaTeX table is also written to LatexFileName
#' @author Scott Hetzel M.S. University of Wisconsin-Madison Biostatistics and
#' Medical Informatics Department
#' @seealso uwLatex()
#' @examples
#' 
#' 
#' weight <- rnorm(100, 200, 10)
#' height <- rnorm(100, 72, 5)
#' bmi <- (weight/(height^2))*703
#' fact <- rep(c("Yes","No"), c(50,50))
#' 
#' df <- data.frame(weight, height, bmi, fact)
#' 
#' uwCorTab(df, yvar="bmi",xvec=c("weight","height"), factor="fact",
#'          LatexFileName=paste(getwd(),"/uwCorTab.tex",sep=""))
#' 
uwCorTab <- function(data,
                     yvar,
                     xvec,
                     factor=NULL,
                     colNames=NULL,
                     printPVals=TRUE,
                     method="spearman",
                     LatexFileName=NULL,
                     returnMat=FALSE,
                     digits=3,
                     ...)
  {
    if(is.null(factor))
       {
         nCol <- ifelse(printPVals,4,3)
       }
    else
      {
        if(nlevels(data[[factor]])!=2){stop("uwCorTab is currently set up for only binary factors in the factor argument")}
        ifelse(printPVals,nCol <- 8,nCol <- 7)
        Fact1 <- subset(data, data[[factor]]==levels(data[[factor]])[1])
        Fact2 <- subset(data, data[[factor]]==levels(data[[factor]])[2])
      }
    matr <- matrix(nrow=length(xvec), ncol=nCol, dimnames=list(NULL,rep("",nCol)))
    matr[,1] <- xvec

    if(is.null(colNames))
      {
        colnames(matr)[1:3] <- c("","N","All")
      }
    else
      {
        colnames(matr) <- colNames
      }

    matAll <- subset(data, select=c(yvar,xvec))

    for(i in 1:length(xvec))
      {
        matr[i,2] <- nrow(na.omit(data.frame(data[[yvar]],data[[xvec[i]]])))
        if(method=="pearson")
          matr[i,3] <- paste(rounds(cor.test(data[[yvar]],data[[xvec[i]]], use="complete.obs",method="pearson",exact=FALSE)$estimate,digits),
                             " (",rounds(cor.test(data[[yvar]],data[[xvec[i]]], use="complete.obs",method="pearson",exact=FALSE)$conf.int[1],digits),
                             ", ",rounds(cor.test(data[[yvar]],data[[xvec[i]]], use="complete.obs",method="pearson",exact=FALSE)$conf.int[2],digits),
                             ")",sep="")
        else if(method=="spearman")
          matr[i,3] <- rounds(cor.test(data[[yvar]],data[[xvec[i]]], use="complete.obs",method="spearman",exact=FALSE)$estimate,digits)
        
        if(is.null(factor))
          {
            if(printPVals)
              {
                matr[i,4] <- rounds(cor.test(data[[yvar]],data[[xvec[i]]], method=method, exact=FALSE)$p.value,3)
                ifelse(matr[i,4]==0,matr[i,4] <- "< 0.001",matr[i,4] <- matr[i,4])
                if(is.null(colNames))
                  {
                    colnames(matr)[4] <- "P-Values"
                  }
              }
          }

        if(!is.null(factor))
          {
            matF1 <- subset(Fact1, select=c(yvar, xvec[i]))
            matF2 <- subset(Fact2, select=c(yvar, xvec[i]))

            matr[i,5] <- rounds(cor(matF1, use="complete.obs",
                                  method=method)[1,,drop=FALSE],digits)[2]
            matr[i,7] <- rounds(cor(matF2, use="complete.obs",
                                  method=method)[1,,drop=FALSE],digits)[2]
            if(is.null(colNames))
              {
                colnames(matr)[4:7] <- c("N",levels(data[[factor]])[1],"N",
                                         levels(data[[factor]])[2])
              }

            NpairsF1 <- sum(!is.na(apply(matF1,1,mean)))
            NpairsF2 <- sum(!is.na(apply(matF2,1,mean)))

            matr[i,4] <- NpairsF1
            matr[i,6] <- NpairsF2

            if(printPVals)
              {
                r1 <- as.numeric(matr[i,5])
                r2 <- as.numeric(matr[i,7])
                n1 <- NpairsF1
                n2 <- NpairsF2
                z1 <- (log(1+r1)-log(1-r1))/2
                z2 <- (log(1+r2)-log(1-r2))/2
                zscore <- abs((z1-z2)/sqrt((1/(n1-3))+(1/(n2-3))))
                matr[i,8] <- round(2*(1-pnorm(zscore)),3)
                ifelse(matr[i,8]=="0", matr[i,8] <- "< 0.001", matr[i,8] <- matr[i,8])
                if(is.null(colNames))
                  {
                    colnames(matr)[8] <- "P-Values"
                  }
              }  
          }
      }
    if(is.null(LatexFileName))
      {
        print(matr)
        warning("LatexFileName is NULL.  Table not saved.")
      }
    else
      {
        uwLatex(matr, file=LatexFileName,cgroup=c("",yvar),n.cgroup=c(1,(ncol(matr)-1)),
                ...)
        if(returnMat){return(matr)}
      }
    
  }

        
