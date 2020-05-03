\name{uwSurvival}
\alias{uwSurvival}
\title{Create Survival Chart and Table}
\description{Charts survival by treatment}
\usage{
uwSurvival(survData,
           statusName,
           trxName,
           trxControl,
           timeName,
           fun=NULL,
           color=NULL,
           pTitle="Survival by Treatment",
           yLab="Percent of Survival Probability",
           xLab="Days from Randomization",
           yLim=NULL,
           markCensor=FALSE,
           Legend=TRUE,
           LegendLoc="topright",
           LegendCex=0.8, 
           printPVals=TRUE,
           pValCex=0.6,
           abbrevN=1,
           printSS=TRUE,
           SSinPlot=FALSE,
           SScex=0.5,
           Ropen=FALSE,
           firstCol.name=NULL,
           GraphSSfreq=1,
           TableSSfreq=GraphSSfreq,
           LatexFileName="survival.tex",
           LatexCaption=NULL,
           numDec=1,
           cexAxis=0.7,
           confInt=FALSE, 
           ...) 
}
\arguments{
  \item{survData}{A dataframe that includes columns for treatment,
    patient status (living/deceased for survival, event/no event for
    other measurements), and the patients' survival days.}
  \item{statusName}{String of the name for the status variable in
    survData}
  \item{trxName}{String of the name for the treatment variable in
    survData}
  \item{trxControl}{String of which level is the control level in
    trxName}
  \item{timeName}{String of the name for the time variable in survData}
  \item{fun}{String as the function utilized for plot.survfit.  Only
    tested for default and "event"}
  \item{color}{Color designations.  Must be equal to nlevels of trxName}
  \item{pTitle}{Title for the graph}
  \item{yLab}{Label for y-axis}
  \item{xLab}{Label for x-axis}
  \item{yLim}{Limits for the y-axis.  Defaults to c(-1, 100) when
    SSinPlot is TRUE and defaults to c(-10, 100) when SS in Plot is FALSE}
  \item{markCensor}{(Logical) TRUE marks where censored data occurs on
    the curves}
  \item{Legend}{(Logical) TRUE plots legend in spot given in LegendLoc}
  \item{LegendLoc}{String of c("topright","top","topleft","left",
    "center","right","bottomright","bottom","bottomleft")}
  \item{LegendCex}{Numeric magnification of the legend size}
  \item{printPVals}{(Logical) TRUE log-rank test p-value is given atop
    the graph}
  \item{pValCex}{Numeric magnification of the p-value print size}
  \item{abbrevN}{(integer) indicating how many letters should
    abbreviation of the treatments levels be when reported in margins.}
  \item{printSS}{(Logical) Prints sample size still at risk at the
    designated time points}
  \item{SSinPlot}{(Logical) TRUE prints sample size at risk inside graph
    itself in negative margins.  Useful when survival is high throughout
    time period}
  \item{SScex}{Numeric magnification of the sample size print size}
  \item{Ropen}{(Logical) TRUE collapses data across treatment levels}
  \item{firstCol.name}{Sting for name of first column in the LaTeX
    tables}
  \item{GraphSSfreq}{Whole number designating how often sample size should be
    shown in the lower margin of the graph}
  \item{TableSSfreq}{Whole number designating how often rows of the
    survfit output should be printed in the LaTex table}
  \item{LatexFileName}{Vector of strings.  Individually give file path
    and name for each LaTeX table which will be constructed for each
    treatment level.}
  \item{LatexCaption}{Vector of strings to be used as the caption for
    the LaTeX tables}
  \item{numDec}{Numeric indicating number of decimal places to be used
    in the x-axis for time of follow-up}
  \item{cexAxis}{Axis label magnifier}
  \item{confInt}{Should confidence interval lines be printed}
  \item{...}{Any other arguments that can be passed on to uwLatex()}
}

\details{
  Additional packages required: library(chron)

  If LaTeX table does not seem to print fully in the document try
  using type="longtable" argument.

  Only works well with layout(1)
}

\examples{

death <- sample(c(0,1), 200, replace=TRUE, prob=c(0.4, 0.6))
trt <- rep(c("ABDC","ABGH"), c(100,100))
time <- ceiling(runif(200, 0, 100))

sData <- data.frame(death,trt,time)
layout(1)
uwSurvival(survData=sData, statusName="death", trxName="trt", trxControl="ABDC",                  
           timeName="time", pTitle="Survival by Treatment",
           yLab="Percent of Survival Probability",
           xLab="Days from Randomization",pValCex=0.8,SScex=0.7,
           markCensor=FALSE, Legend=TRUE, LegendLoc="topright",   
           LatexFileName=c(paste(getwd(),"surv1.tex",sep=''),
                           paste(getwd(),"/surv2.tex",sep='')),
           showTab=FALSE,printPVals=TRUE,printSS=TRUE,
           Ropen=FALSE,GraphSSfreq=5,TableSSfreq=1,firstCol.name=NULL,
           LatexCaption=NULL,SSinPlot=TRUE,
           size="scriptsize")

}
\author{Scott Hetzel, M.S. Department of Biostatistics and Medical
  Informatics. University of Wisconsin-Madison.
}


