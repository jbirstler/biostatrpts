% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r2lme.R
\name{r2lme}
\alias{r2lme}
\title{Calculates R-squared for lme model}
\usage{
r2lme(obj)
}
\arguments{
\item{obj}{lme object}
}
\value{
R-square value
}
\description{
Calculates R-squared for lme model
}
\details{
implements R2 from Edwards, Muller, Wolfinger, Qaqish and Schabenberger,
2008.  need the aod package for wald.test function
}
