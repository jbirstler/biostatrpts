

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
