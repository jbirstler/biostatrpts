#' Scatter Plot with Simple Linear Regression Line
#' 
#' This function creates a scatter plot of two continuous variables and adds
#' the fitted simple linear regression line.
#' 
#' 
#' @param Data Data frame with response and dependent variable
#' @param respVar String of the response variable's name in Data
#' @param depVar String of the dependent variable's name in Data
#' @param printPVal Logical: TRUE indicates p-value for beta1 = 0 is printed in
#' margin of plot
#' @param printModel Logical: TRUE indicates fitted model is printed in margin
#' of plot
#' @param printRsq Logical: TRUE indicates multiple R-squared is printed in
#' margin of plot
#' @param returnList Logical: TRUE returns list of Coefficient and P-Value
#' @param ... Any other arguments that can be used by plot()
#' @return Plots a scatterplot and regression line.  Returns list of beta1
#' coefficient and p-value
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso plot(), abline(), lm()
#' @examples
#' 
#' 
#'   Xvar <- rnorm(50,-10,1)
#'   Yvar <- 0.5*Xvar + rnorm(50,0,1)
#' 
#'   dfLin <- data.frame(Xvar,Yvar)
#'  
#'   layout(1)
#'   uwSimpleReg(Data=dfLin,respVar="Yvar",depVar="Xvar",
#'               main="Example of uwSimpleReg",
#'               ylab="Example Response", xlab="Example Explanatory",
#'               ylim=NULL,xlim=NULL, printPVal=TRUE,printModel=TRUE,printRsq=TRUE)
#' 
uwSimpleReg <- function(Data,
                        respVar,
                        depVar,
                        printPVal=TRUE,
                        printModel=TRUE,
                        printRsq=TRUE,
                        returnList=TRUE, ...)
{
lm1 <- lm(Data[[respVar]] ~ Data[[depVar]], data=Data)

plot(x=Data[[depVar]],
     y=Data[[respVar]],
     tcl=NA, ...)

box()
abline(lm1)

if(printPVal)
  {
    pval <- round(summary(lm1)$coef[2,4],3)
    if(pval == 0)
      {mtext("p < 0.001", side=3, line=0, cex=0.7)}
    else
      mtext(paste("p = ",pval,sep=''), side=3, line=0, cex=0.7)
  }
if(printModel)
  {
    model <- paste("Fitted Regression Line: Y = ",
                   round(summary(lm1)$coef[2,1],3),"*X ",
                   if(summary(lm1)$coef[1,1] < 0){round(summary(lm1)$coef[1,1],3)}
                   else{paste("+ ",round(summary(lm1)$coef[1,1],3), sep="")},
                   sep='')
    mtext(model, side=3, line=0.8, cex=0.7)
  }
if(printRsq)
  {
    mtext(paste("R squared = ",round(summary(lm1)$r.squared,3),sep=''),
          side=1,cex=0.7)
  }
if(returnList)
  {
    return(list(Coef=round(summary(lm1)$coef[2,1],4), PVal=summary(lm1)$coef[2,4]))
  }
}
