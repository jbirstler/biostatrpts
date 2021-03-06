% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rounds.R
\name{rounds}
\alias{rounds}
\title{A function to round to a certain decimal but keep the zero as the last
decimal spot if necessary}
\usage{
rounds(x, digits = 0, as.numeric = FALSE)
}
\arguments{
\item{x}{A number or numeric vector whose digits will be rounded}

\item{digits}{Number of decimal places to round to}

\item{as.numeric}{If FALSE the number returned is a character string.  If
TRUE the number returned is numeric}
}
\value{
A character or numeric string or vector
}
\description{
This function is used throughout multiple biostatrpts functions instead of
the traditional round() function.
}
\details{
Embedded into multiple biostatrpts functions to round values neatly and have
consistent reporting.
}
\examples{

#  See the difference in the print out of the two round functions for
#  rounding 0.0395 when rounding to 3 decimal places

rounds(0.0395, 3)
round(0.0395, 3)
}
\seealso{
round()
}
\author{
University of Wisconsin-Madison Biostatistics and Medical
Informatics Department, Scott Hetzel M.S.
}
