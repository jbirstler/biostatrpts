% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uwBar.R
\name{uwBar}
\alias{uwBar}
\title{Bar Plot function embedded in other bar plot functions}
\usage{
uwBar(
  table,
  pBeside = TRUE,
  pHoriz = FALSE,
  lined = FALSE,
  axLim = NULL,
  yLab = NULL,
  xLab = NULL,
  printBarVals = TRUE,
  Legend = FALSE,
  LegendLoc = "topright",
  LegendCex = 0.8,
  mgp.y = c(3, 1, 0),
  mgp.x = c(3, 1, 0),
  cex.names = 1,
  barNamesLas = 1,
  barNamesSeq = 1,
  barNamesAngle = 0,
  digits = 1
)
}
\arguments{
\item{table}{table or matrix used for plotting bar graph}

\item{pBeside}{(barplot)(logical) Stacked or side-by-side bars}

\item{pHoriz}{(barplot)(logical) Horizontal or vertical bars}

\item{lined}{(logical) Lined or solid colored bars}

\item{axLim}{(vector) Min and max axis values; x-axis for pHoriz=TRUE,
y-axis for pHoriz=FALSE}

\item{yLab}{(barplot)(string) Y-axis label}

\item{xLab}{(barplot)(string) X-axis label}

\item{printBarVals}{(logical) If TRUE the value of each bar is printed above
each bar}

\item{Legend}{(logical) If TRUE a legend is printed}

\item{LegendLoc}{(legend) Coordinates of string location of printed legend
if Legend=TRUE.  See ?legend}

\item{LegendCex}{(legend)(numeric) Sizing ratio of printed legend if
Legend=TRUE}

\item{mgp.y}{(par)(vector) Y-axis margin lines for title, label, and line}

\item{mgp.x}{(par)(vector) X-axis margin lines for title, label, and line}

\item{cex.names}{(barplot)(numeric) Sizing ratio of bar names}

\item{barNamesLas}{(0,1,2,or 3). Changes the verticality of the treatment
names for bar names and numeric axis. 1 and 2 are best for reporting}

\item{barNamesSeq}{(interger) Number indicating which bar labels to print.
This number is given as the 'by' attribute to seq().  So every barNameSeq
would be printed.  Default is 1, which will print all bar labels.  Helpful
when bar labels overlap when printing all of them.}

\item{barNamesAngle}{(integer) Number between 0 and 360 indicating how the
margin names should be rotated.  Useful when margin names are long strings
so you can angle them to all be seen}

\item{digits}{(rounds)(numeric) Number of decimal places to round to}
}
\value{
Returns a list of (1) a matrix of the locations of each bar, (2) the
data used to create the bar plot, and (3) the axis values

These values are used in the other biostatrpt functions for other items that
are printed like p-values.
}
\description{
Function called by other bar plot functions to actually create the bar plot.
This allows for a consistent look throughout multiple different functions
}
\details{
Additional Functions Required: rounds()

This function is an adaptation of the traditional barplot() in which axes,
bars, bar values, and legends have the ability to be formatted in multiple
different ways.

uwBar is called by any biostatrpts function that creates bar plots.
}
\examples{


mat <- matrix(1:12, nrow = 3)
dimnames(mat) <- list(c("A", "B", "C"), c("D", "E", "F", "G"))

uwBar(mat,
  pBeside = TRUE, pHoriz = FALSE, lined = FALSE,
  axLim = NULL, yLab = NULL, xLab = NULL, printBarVals = TRUE,
  Legend = TRUE, LegendLoc = "topright", LegendCex = 0.8,
  mgp.y = c(3, 1, 0), mgp.x = c(3, 1, 0), cex.names = 1, barNamesLas = 1,
  barNamesSeq = 1, barNamesAngle = 0, digits = 1
)
}
\author{
University of Wisconsin-Madison Biostatistics and Medical
Informatics Department, Scott Hetzel M.S.
}
