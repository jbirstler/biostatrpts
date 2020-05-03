

\name{uwDiff}
\alias{uwDiff}
\title{Function for finding differences between different factor levels
  for the same patient for either numeric or ordinal variables}
\description{This function is meant for finding differences between
  different factor levels for the same patient for either numeric or
  ordinal variables.  It is utilized in uwLeveledBoxPlot.
 }
\usage{
uwDiff(data,
       varvec,
       trxName=NULL,
       VisitName,
       Baseline,
       ptID,
       deltaPct=FALSE,
       digits=1)
}
\arguments{
\item{data}{Name of data frame in R}
\item{varvec}{String or vector of variable name(s) of interest in data}
\item{trxName}{String of treatment variable name in data.  It is just
  copied over to the new data frame}
\item{VisitName}{String of variable name for time points}
\item{Baseline}{String of level name in VisitName that is the baseline
  value.  This must be the first level in VisitName too.}
\item{ptID}{String of variable name for Identifying unique subjects}
\item{deltaPct}{Logical. TRUE gives percentage change from baseline.
  FALSE gives differences from baseline}
\item{digits}{Numeric.  Number of decimals in the percentages if
  deltaPct=TRUE}
}
\value{
A data frame is returned with ID column, time column, treatment
column, and difference column(s).

Differences are set so the baseline value is being subtracted from the
non-baseline value.

Column names in difference data frame are the same names as that are
given in the arguments of uwDiff.
}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.  Assistance from Frontier
  Science and Technology Reseach Foundation, Pat Lenon and Zekai Otles.
}

\seealso{uwLeveledBoxPlot()}
\examples{
## Example

ID <- rep(letters[1:20], 3)
TRT <- rep(c("A","B"), 30)
Time <- c(rep("Baseline",20),rep("Week 5",20),rep("Week 10",20))
Time <- ordered(Time, c("Baseline", "Week 5", "Week 10"))
AE <- sample(c(1,2,3,4),60,replace=TRUE)
  
data2 <- data.frame(ID,TRT,Time,AE)

uwDiff(data=data2,
       varvec="AE",
       trxName="TRT",
       VisitName="Time",
       Baseline="Baseline",
       ptID="ID",
       deltaPct=TRUE,
       digits=1)

}
