\name{compSweave}
\alias{compSweave}
\title{Compile Sweave .Rnw file}
\description{Compile Sweave. Rnw file into a PS file using latex and dvips}

\usage{
compSweave(filename,
           ...)
}
\arguments{
  \item{filename}{String (without .Rnw extension) giving name of .Rnw
    file in the working directory of R to be compiled into a PDF by
    Sweave() and pdflatex}
  \item{...}{Any argument to be passed to Sweave()}
}

\value{
  Output is a PS file of the name 'filename'.ps created in the
  directory that is the working directory in R
}

\details{
  First compiles the .Rnw file using Sweave() and then creates the postscript
  file using CMD latex twice and CMD dvips once to make sure table of
  contents and other references are correct.  Use CMD ps2pdf once in
  terminal to turn into a PDF.
}

\author{University of Wisconsin-Madison Biostatistics and Medical
  Informatics Department, Scott Hetzel M.S.}

\seealso{beginSweaveFile(), Sweave()}

\examples{
compSweave("report")

#Make sure there is a file name "report.Rnw" in
#the R working directory.  If there are syntax errors in the R or LaTeX
#code then document will not be fully compiled.
}
