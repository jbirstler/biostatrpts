\name{meanFunc}
\alias{meanFunc}
\title{Format for printing mean (SD) by factor}
\description{Meant to be used in creating R matrix to be exported as a table}

\usage{
meanFunc(data,
         nvar,
         gvar=NULL,
         digits=1)
}
\arguments{
  \item{data}{Data frame where nvar and gvar can be found}
  \item{nvar}{Character string of column in data of which means will be calculated}
  \item{gvar}{Character string of column in data of which means will be
    grouped by}
  \item{digits}{Number of decimals the numbers will be rounded to}
}

\value{
  Returns a string of the format mean (SD) for each of the levels in
  gvar. If gvar is null then mean (SD) of nvar is provided.
}

\details{
  na.rm=TRUE is defaulted in the calculation of mean and SD
}

\seealso{medianFunc}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.}

\examples{
outcome <- rnorm(20)
group <- factor(rep(c("A","B"),each=10))
d.frame <- data.frame(outcome,group)

meanFunc(d.frame,"outcome","group")
}
