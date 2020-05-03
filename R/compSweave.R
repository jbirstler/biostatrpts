#' Compile Sweave .Rnw file
#' 
#' Compile Sweave. Rnw file into a PS file using latex and dvips
#' 
#' First compiles the .Rnw file using Sweave() and then creates the postscript
#' file using CMD latex twice and CMD dvips once to make sure table of contents
#' and other references are correct.  Use CMD ps2pdf once in terminal to turn
#' into a PDF.
#' 
#' @param filename String (without .Rnw extension) giving name of .Rnw file in
#' the working directory of R to be compiled into a PDF by Sweave() and
#' pdflatex
#' @param ... Any argument to be passed to Sweave()
#' @return Output is a PS file of the name 'filename'.ps created in the
#' directory that is the working directory in R
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso beginSweaveFile(), Sweave()
#' @examples
#' 
#' compSweave("report")
#' 
#' #Make sure there is a file name "report.Rnw" in
#' #the R working directory.  If there are syntax errors in the R or LaTeX
#' #code then document will not be fully compiled.
#' 
compSweave <- function(filename, ...)
  {
    Sweave(paste(filename,'.Rnw',sep=""), stylepath=TRUE, ...)
    system(paste('latex',' ',filename,sep=""))
    system(paste('latex',' ',filename,sep=""))
    system(paste('dvips',' ',filename,sep=""))
  }
