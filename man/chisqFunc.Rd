\name{chisqFunc}
\alias{chisqFunc}
\title{Format for printing p-value from Chi-square or Fisher exact test}
\description{Meant to be used in creating R matrix to be exported as a table}

\usage{
chisqFunc(data,
         fvar,
         gvar,
         fisher=FALSE)
}
\arguments{
  \item{data}{Data frame where nvar and gvar can be found}
  \item{fvar}{Character string of column in data of factor data}
  \item{gvar}{Character string of column in data of grouping data}
  \item{fisher}{Logical whether to use Fisher exact test or chi-square}
}

\value{
  Returns a string of the p-value from a Chi-square test or
  Fisher exact test
}

\details{
  If the p-value is less than 0.0005 then "< 0.001" is returned
  otherwise p-value rounded to 3 decimals is returned
}

\seealso{tableFunc}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.}

\examples{
set.seed(784)
outcome <- factor(sample(c("No","Yes"),replace=TRUE,size=20))
group <- factor(rep(c("A","B"),each=10))
d.frame <- data.frame(outcome,group)

chisqFunc(d.frame,"outcome","group",fisher=TRUE)
}
