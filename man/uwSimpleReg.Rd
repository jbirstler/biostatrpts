\name{uwSimpleReg}
\alias{uwSimpleReg}
\title{Scatter Plot with Simple Linear Regression Line}
\description{This function creates a scatter plot of two continuous
  variables and adds the fitted simple linear regression line.}
\usage{uwSimpleReg(Data,
                   respVar,
                   depVar,
                   printPVal=TRUE,
                   printModel=TRUE,
                   printRsq=TRUE,
                   returnList=TRUE, ...)
}
\arguments{
  \item{Data}{Data frame with response and dependent variable}
  \item{respVar}{String of the response variable's name in Data}
  \item{depVar}{String of the dependent variable's name in Data}
  \item{printPVal}{Logical:  TRUE indicates p-value for beta1 = 0 is
    printed in margin of plot}
  \item{printModel}{Logical: TRUE indicates fitted model is printed in
    margin of plot}
  \item{printRsq}{Logical:  TRUE indicates multiple R-squared is printed
    in margin of plot}
  \item{returnList}{Logical: TRUE returns list of Coefficient and P-Value}
  \item{...}{Any other arguments that can be used by plot()}

}

\value{Plots a scatterplot and regression line.  Returns list of beta1
  coefficient and p-value}


\seealso{plot(), abline(), lm()}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.}


\examples{

  Xvar <- rnorm(50,-10,1)
  Yvar <- 0.5*Xvar + rnorm(50,0,1)

  dfLin <- data.frame(Xvar,Yvar)
 
  layout(1)
  uwSimpleReg(Data=dfLin,respVar="Yvar",depVar="Xvar",
              main="Example of uwSimpleReg",
              ylab="Example Response", xlab="Example Explanatory",
              ylim=NULL,xlim=NULL, printPVal=TRUE,printModel=TRUE,printRsq=TRUE)
}
