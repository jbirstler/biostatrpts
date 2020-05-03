\name{uwBarPlot}
\alias{uwBarPlot}
\title{Bar Plots for various conditions}
\description{Bar graph for one or two categorical variables.  trxName is
  on the outside of the graph and factorName is on the inside of the
  graph. uwBar is used to create the bar plot. uwPVals calculates the
  p-values. uwLatex is used to create a corresponding LaTeX table.}
\usage{
uwBarPlot(demoData,
          demoBarPlot=FALSE,
          trxName=NULL,
          factorName,
          factorNames=NULL,
          cex.names=1,
          pBeside=TRUE,
          pHoriz=FALSE,
          yLab=NULL,
          xLab=NULL,
          pTitle=NULL,
          axLim=NULL,
          printBarVals=TRUE,
          digits=1,
          lined=TRUE,
          Percent=TRUE,
          barNamesLas=1,
          barNamesSeq=1,
          barNamesAngle=0,
          printPVals=TRUE,
          pTest=c("wilcox","fisher","chisq"),
          abbrevN=1,
          pairwise=TRUE,
          pAdjust=NULL,
          trxControl=NULL,
          pExclude=NULL,
          pInclude=list(list(NULL,NULL)),
          pStrWrap=FALSE,
          pValCex=0.7,
          titleCex=1,
          Legend=TRUE,
          LegendLoc="topright",
          LegendCex=0.8,
          LatexFileName=NULL,
          LatexCaption=NULL,
          trxColname=NULL,
          ...)

}
\arguments{
  \item{demoData}{data frame containing 1 or 2 factor columns}
  \item{demoBarPlot}{(logical) TRUE plots a bar plot like the old
    uwDemoBarPlot function in which each factorName level is broken down
  by trxName.  FALSE plots a traditional uwBarPlot in which each trxName
level is broken down by factorName}
  \item{trxName}{(string) Name of the demoData column containing the
    treatment factor. If NULL then bar plot for just factorName is
    given} 
  \item{factorName}{(string) Name of the demoData column containing
    the categorical factor.}
  \item{factorNames}{Vector of names to be applied to the factor
    column. Be careful with order.  Table is created with factorName
    levels, which can then be switched if factorNames is not NULL.
    Length must be the same as number of levels of factorName. If
    factorName is ordered follow that ordering when listing the order
    of factorNames, if not then follow alphabetic ordering of levels
    of factorName.}
  \item{cex.names}{(uwBar) expansion factor for axis names (bar
    levels)}
  \item{pBeside}{(uwBar)(logical) If TRUE the graph is a stacked bar
    plot.}
  \item{pHoriz}{(uwBar)(logical) If TRUE the bars are horizontal to the
    x-axis.}  
  \item{yLab}{(uwBar)(string) Label of y-axis.}
  \item{xLab}{(uwBar)(string) Label of x-axis.}
  \item{pTitle}{(string) Title of the graph.}
  \item{axLim}{(c(start, finish)) Gives limit of axis for bar
    heights. x-axis if pHoriz=TRUE, y-axis if pHoriz=FALSE} 
  \item{printBarVals}{(uwBar)(logical) If TRUE the value represented by
    each bar is printed over the top of the bar. Only if pBeside=TRUE}
  \item{digits}{(positive numeric) Indicates number of decimals to be
    reported in the graph and in the table}
  \item{lined}{(uwBar)(logical) TRUE has levels of bars alternate between
    solid and lined bars.}
  \item{Percent}{(logical) If TRUE the graphs show percentages of
    patients instead of counts.}
  \item{barNamesLas}{(0,1,2,or 3). Changes the verticality of the
    treatment names for bar names and numeric axis. 1 and 2 are best for
    reporting}
  \item{barNamesSeq}{(uwBar)(integer) Number indicating which bar
    labels to print.  This number is given as the "by" attribute to
    seq().  So every barNameSeq would be printed.  Default is 1, which
    will print all bar labels.  Helpful when bar labels overlap when
    printing all of them.}
  \item{barNamesAngle}{(uwBar)(integer) Number between 0 and 360
    indicating how the margin names should be rotated.  Useful when
    margin names are long strings so you can angle them to all be seen}
  \item{printPVals}{(uwPVals)(logical) If TRUE, p-values indicated by
    the attributes for uwPVals are printed in the margin of the plot.}
  \item{pTest}{(uwPVals)(printPVals=TRUE) Chose type of comparison test}
  \item{abbrevN}{(uwPVals)(integer) indicating how many letters should
    abbreviation of the treatments levels be when reported in margins.}
  \item{pairwise}{(uwPVals)(logical) If TRUE, pairwise comparisons
    should be made between treatment levels.  If FALSE, multi-level test
    occurs between all levels of the treatment (i.e. ANOVA)}
  \item{pAdjust}{(uwPVals)(pairwise=TRUE) NULL for none, "h" for "holm",
    or "b" for "bonferroni". P-value adjustment methods.} 
  \item{trxControl}{(uwPVals)(pairwise=TRUE)  If NULL, all two-way
    tests will be examined.  If a treatment level is indicated here as
    the control level, then only two-way comparisons with the control and
    non-control levels will be made.}
  \item{pExclude}{(uwPVals)(pairwise=TRUE) If trxControl is NULL then
    this should be a vector of treatment levels, looked at two at a
    time, that indicate that the p-value for that contrast should NOT be
    reported.  Suppose the treatment levels are A,B,C,D and you do not 
    want the comparisons for B.C., B.D., and C.D..  pExclude should look
    like this.  pExclude=c("B","C","B","D","C","D"). If trxControl is
    defined then this should be a vector of non-control treatment levels
    that will be looked at one at a time and that non-control's contrast will
    NOT be reported}
  \item{pInclude}{(uwPVals)(pairwise=TRUE) Gives the user the ability to
    specify which contrasts they would like to get p-values for.  The
    ability to combine treatment levels for a contrast is possible.  See
    help(uwPVals) for more details.} 
  \item{pStrWrap}{(logical) If TRUE, creates two rows of p-value string on
    the top margin instead of one.  Useful for many reported p-values.}
  \item{pValCex}{Number between 0 and 1. Size of the p-value font in the
    top margin.}
  \item{titleCex}{(figureBox) Number between 0 and 1.  Size of the title font}
  \item{Legend}{(uwBar)(logical) If TRUE, puts a legend in the graph}
  \item{LegendLoc}{(uwBar)(string)(Legend=TRUE). Gives the location in
    the graph where the legend will be located. One of these must be
    given. "topleft", "top", "topright", "left", "center", "right",
    "bottomleft", "bottom", or "bottomright"}  
  \item{LegendCex}{Size of legend. Number between 0 and 1, 1=full size,
    0=no size.} 
  \item{LatexFileName}{(uwLatex)(string) Giving the folder and
    file(ending in .tex) for the LaTeX table to be saved. If NULL, table
    will be printed, but not saved.} 
  \item{LatexCaption}{(uwLatex)(string) Giving the caption for each LaTeX table.
    If NULL, defaults to pTitle}
  \item{trxColname}{(uwLatex)(string) Name for first column, which is
    like a title for the rows.  The default is trxName if
    !is.null(trxName), or factorName if is.null(trxName).} 
  \item{...}{Any other argument that can be passed to uwLatex()}
}
\details{

Additional Functions Required:  uwLatex(), uwPVals(), uwBar(), and figureBox()

factorName must be a column name in demoData and a factor or able to
be converted to a factor 
trxName, if given, must be a column name in demoData and a factor or
able to be converted to a factor.

It is recommended that factorName is an ordered vector especially
when it is an ordinal variable.  This will ensure proper order in the
graph and table. 

With printPVals==TRUE, see documentation for uwPVals() to see how to use
pTest, pairwise, trxControl, and pExclude effectively

With printPVals==TRUE and trxControl=NULL, all two-way comparisons are reported for the
levels of trxName.  No adjustments are made to the p-values to account
for the multiple testing.  pAdjust can be utilized to make p-value
adjustments.
}

\examples{

TRT <- rep(c("A","B","C"), 30)
AE <-ordered(sample(c("None","Mild","Moderate","Severe"),90,replace=TRUE),
             c("None", "Mild", "Moderate", "Severe"))
data <- data.frame(TRT,AE)

layout(matrix(c(1,1,2,2,3,3,4,4),nrow=2,byrow=TRUE))
uwBarPlot(demoData=data,trxName=NULL,factorName="AE",Legend=TRUE,demoBarPlot=FALSE,pHoriz=TRUE,pBeside=TRUE,
          xLab="Number of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=60,cex.names=0.8,
          lined=FALSE,Percent=FALSE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
          LatexFileName=NULL,LegendLoc="topright")

uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=TRUE,pHoriz=TRUE,
          xLab="Number of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=0,
          lined=FALSE,Percent=FALSE,barNamesLas=0,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
          LatexFileName=NULL,LegendLoc="topright",pStrWrap=TRUE)

uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=FALSE,pHoriz=FALSE,
          yLab="Percent of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=0,
          lined=FALSE,Percent=TRUE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
          LatexFileName=NULL,LegendLoc="topleft")

uwBarPlot(demoData=data,trxName="TRT",factorName="AE",Legend=TRUE,demoBarPlot=TRUE,pHoriz=FALSE,
          yLab="Percent of Subjects",pTitle="Adverse Events",digits=1,barNamesAngle=45,
          lined=TRUE,Percent=TRUE,barNamesLas=1,barNamesSeq=1,printPVals=TRUE,pTest="f",pValCex=0.6,
          LatexFileName=NULL,LegendLoc="topleft")
}
\author{University of Wisconsin-Madison Biostatistics and Medical Informatics Department, Scott Hetzel M.S.}

