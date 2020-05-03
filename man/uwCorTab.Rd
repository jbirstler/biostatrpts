
\name{uwCorTab}
\alias{uwCorTab}
\title{Correlation Tables}
\description{This function is meant for creating correlation tables of
  one variable with multiple other variables}
\usage{
uwCorTab(data,
         yvar,
         xvec,
         factor=NULL,
         colNames=NULL,
         printPVals=TRUE,
         method="spearman",
         LatexFileName,
         returnMat=FALSE,
         digits=3,
         ...)
}
\arguments{
  \item{data}{The data frame that all the variables are found}
  \item{yvar}{String of the name in the data frame for the "response"
    variable}
  \item{xvec}{Vector of strings giving the names in the data frame for
    the other variables to be correlated with yvar}
  \item{factor}{Vector for a two-level variable that can be used to
    seperate the data and have a test for differences in correlations
    based on Fisher's Z-transformation}
  \item{colNames}{Manually put in the names for the columns of the table
    produced. If factor is null and printPVals is FALSE ncol=3.  If
    factor is null and printPVals is TRUE ncol=4.  If factor is not null
    and printPVals is FALSE ncol=7 (printPVals TRUE, ncol=8).}
  \item{printPVals}{Logical: Is forced to FALSE if no factor is
    provided}
  \item{method}{Method to be used in the call of the cor() function}
  \item{LatexFileName}{File name for the LaTeX table that is created
    using uwLatex()}
  \item{returnMat}{Logical: Do you want the matrix to be printed in R}
  \item{digits}{Number of decimals to be printed}
  \item{...}{Other arguments that can be passed to uwLatex().  cgroup
    and n.cgroup is already set}
}
\value{
  Output is the matrix that is created and used by uwLatex().  Code for
  the LaTeX table is also written to LatexFileName
}
\author{Scott Hetzel M.S. University of Wisconsin-Madison Biostatistics
  and Medical Informatics Department}

\seealso{uwLatex()}

\examples{

weight <- rnorm(100, 200, 10)
height <- rnorm(100, 72, 5)
bmi <- (weight/(height^2))*703
fact <- rep(c("Yes","No"), c(50,50))

df <- data.frame(weight, height, bmi, fact)

uwCorTab(df, yvar="bmi",xvec=c("weight","height"), factor="fact",
         LatexFileName=paste(getwd(),"/uwCorTab.tex",sep=""))
}
