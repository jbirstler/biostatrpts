\name{diffFunction}
\alias{diffFunction}
\title{Function for finding differences between different factor levels
  for the same patient for either numeric or ordinal variables}
\description{This function is meant for finding differences between
  different factor levels for the same patient for either numeric or
  ordinal variables.  It can be used inside the AEfactorized() and
  uwLeveledBoxPlot by setting delta=TRUE, or by itself
 }
\usage{
diffFunction(Diffdata,
             variable,
             TrxName,
             VisitName,
             Baseline,
             ptID,
             deltaPct=FALSE)
}
\arguments{
\item{Diffdata}{Name of data set in R}
\item{variable}{String of variable name of interest in Diffdata}
\item{TrxName}{String of treatment variable name in Diffdata}
\item{VisitName}{String of variable name for time points}
\item{Baseline}{String of level name in VisitName to be used as baseline}
\item{ptID}{String of variable name for Identification Numbers}
\item{deltaPct}{Logical. TRUE gives percentage change from baseline}
}
\value{
A data frame is returned with difference column, time column, treatment
column, and ID column.

Differences are set so the baseline value is being subtracted from the
non-baseline value.
}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.  Assistance from Frontier
  Science and Technology Reseach Foundation, Pat Lenon and Zekai Otles.
}

\seealso{AEfactorized(), uwLeveledBoxPlot()}
\examples{
## Example

ID <- rep(letters[1:20], 3)
TRT <- rep(c("A","B"), 30)
Time <- c(rep("Baseline",20),rep("Week 5",20),rep("Week 10",20))
Time <- ordered(Time, c("Baseline", "Week 5", "Week 10"))
AE <- sample(c(1,2,3,4),60,replace=TRUE)
  
data2 <- data.frame(ID,TRT,Time,AE)

diffFunction(Diffdata=data2,
             variable="AE",
             TrxName="TRT",
             VisitName="Time",
             Baseline="Baseline",
             ptID="ID",
             deltaPct=TRUE)

}
