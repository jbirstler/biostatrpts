\name{uwBoxPlot}
\alias{uwBoxPlot}
\title{Box Plots of Numeric Variables}
\description{This function is meant for the creation of box plots for
  numeric variables, also allowing for comparisons of box plots through
  a categorical variable.  This function also produces the LaTeX code
  needed to create the corresponding LaTeX table summarizing the box
  plots numerically.}
\usage{
uwBoxPlot(data,
          trxName=NULL, 
          metricName,
          LatexFileName=NULL,
          pTitle=NULL,
          yLab=NULL,
          xLab=NULL,
          plotMean=TRUE, pOutliers=TRUE,
          boxWex=0.75,
          Ropen=FALSE,
          printPVals=TRUE,
          pTest=c("wilcox","t.test"),
          abbrevN=1,
          pairwise=TRUE,
          pAdjust=NULL,
          trxControl=NULL,
          pExclude=NULL,
          pInclude=list(list(NULL,NULL)),
          yLim=NULL,
          pStrWrap=FALSE,
          pValCex=0.7,
          titleCex=1,
          LatexCaption=NULL,
          firstCol.name=NULL,
          digits=1,
          cex.names=0.7,
          ...)
}
\arguments{

  \item{data}{Data frame with relevant variables.}
  \item{trxName}{String of the variable name that represents the
    treatment in data}
  \item{metricName}{String of the variable name that represents the
    numeric variable in data}
  \item{LatexFileName}{(string) Giving the folder and file(ending in
    .tex) for the LaTeX table to be saved. The default is NULL, which
    will create a table in the current directory of the R session, and
    will have the file name of the metricName.tex.}
  \item{pTitle}{String of the Title of the plot.}
  \item{yLab}{String of the y-axis label}
  \item{xLab}{String of the x-axis label}
  \item{plotMean}{Logical. TRUE -> Gives a line in the box plot that
    shows where the mean is.}
  \item{pOutliers}{Logical.  TRUE -> Plots outliers}
  \item{boxWex}{Numeric between 0 and 1 for width of boxes.}
  \item{printPVals}{Logical. Do you want P-values to be reported?}
  \item{pTest}{uwPVals(), "wilcox" or "t.test", which test do you want
    to use?  Wilcoxon Rank Sum Test or Student's T-test? First letter
    only is sufficient.}
  \item{abbrevN}{uwPVals(), Whole number indicating how many letters should
    abbreviation of the treatments levels should be.}
  \item{pairwise}{uwPVals(), (logical) TRUE pairwise comparisons should be made
    between treatment levels.  FALSE multi-level test occurs between all
    levels of the treatment}
  \item{pAdjust}{NULL for none, "h" for "holm", or "b" for "bonferroni".
    P-value adjustment methods.  See uwPVals for more information.}
  \item{trxControl}{uwPVals(), only applies if pairwise=TRUE.  If NULL, all two-way
    tests will be examined.  If a treatment level is indicated here as
    the control level, then only two-way comparisons with the control and
    non-controls will be made.}
  \item{pExclude}{uwPVals(), only applies if pairwise=TRUE and pInclude
    is NULL. If trxControl is NULL
    then this should be a vector of treatment levels, looked at two at a
    time, that indicate that the p-value for that contrast should not be
    reported.  Suppose the treatment levels are A,B,C,D and you do not
    want the comparisons for B.C., B.D., and C.D..  pExclude should look
    like this.  pExclude=c("B","C","B","D","C","D"). If trxControl is
    defined then this should be a vector of non-control treatment levels
    that will be looked at one at a time and that non-control's contrast will
    not be reported}
  \item{pInclude}{uwPVals(), only applies if pairwise=TRUE.  Gives
    the user the ability to specify which contrasts they would like
    to get p-values for.  The ability to combine treatment levels for a
    contrast is possible.  See help(uwPVals) for more details.}
  \item{yLim}{Limits of the y-axis. In format: c(start, finish)}
  \item{pStrWrap}{Logical. TRUE -> Creates two rows of p-value string on
    the top margin instead of one.  Useful for many reported p-values.}
  \item{pValCex}{Number between 0 and 1. Size of the p-value font in the
    top margin.}
  \item{titleCex}{Number between 0 and 1.  Size of Title text}
  \item{LatexCaption}{String to be used as the caption for the LaTeX
    table.  Defaults to metricName.}
  \item{firstCol.name}{(string) Name for first column, which is like a
    title for the rows.  The default is TrxName if !Ropen, or
    catFactorName if Ropen.}
  \item{digits}{Numeric between 0 to 20 indicating number of decimals to
    be reported in the tables}
  \item{cex.names}{cex for size of treatment labels on x-axis}
  \item{...}{Any other argument that can be passed to uwLatex}
}
\details{Uses uwPVals() to calculate p-values and uwLatex() to create
  the LaTeX table)

  P-values do not have multiple comparison adjustments}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S. and Frontier Science and
  Technology Research Foundation, Patrick Lenon and Zekai Otles}

\examples{

TRT <- c(rep("A",20), rep("B",20), rep("C",20))
metric <- c(rnorm(20,2,1), rnorm(20,3,1), rnorm(20,4,1))

dat <- data.frame(TRT,metric)

uwBoxPlot(data=dat,
             trxName="TRT", 
             metricName="metric",
             LatexFileName=NULL,
             showTab=FALSE,
             pTitle="Example of uwBoxPlot",
             yLab="Weight of Something (oz)", 
             plotMean=FALSE, pOutliers=FALSE,
             boxWex=0.75,
             Ropen=FALSE,
             printPVals=TRUE,
             pTest="t",
             abbrevN=1,
             pairwise=TRUE,
             trxControl=NULL,
             pExclude=NULL,
             pInclude=list(list(c("A","B"),"C"), list("B","C"),
                      list("A","C")),
             yLim=NULL,
             pStrWrap=FALSE,
             pValCex=0.7,
             LatexCaption=NULL,
             col.just=c("|l|",rep("c",10),"|r","r|"),
             firstCol.name=NULL
            ) 
}
