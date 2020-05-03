#' Create Tex code to input LaTeX tables
#' 
#' This function is meant to be used in a .Rnw Sweave document to create the
#' LaTeX code input{...} necessary to place tables in the final document.
#' Arguments in R chunk in the .Rnw file must be results=tex and include=FALSE
#' 
#' Meant to be implemented in the compiling of a .Rnw file by Sweave() command.
#' 
#' @param texName Vector of strings indicating the name of the .tex file that
#' holds the LaTeX code for creating the table
#' @param clearpage Logical: TRUE indicates clearpage will be added to the
#' LaTeX code after all of the input{...} statements are written
#' @param multicols Logical: TRUE indicates that the tables given in texName
#' should be oriented into multiple columns on the page.  This means that the
#' 'begin' statement of all of the tables in texName must be of the format
#' begin{tablehere} and likewise for the 'end' statement.  Default is FALSE
#' @param numCols Numeric indicating number of columns the tables should be put
#' into
#' @return input{texName[i].tex} will be placed into .tex file created when
#' Sweave is run
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso Sweave()
#' @examples
#' 
#' # Not Run
#' #
#' # First calling a R chunck in Sweave language
#' #    <<tex, results=tex, include=FALSE>>=
#' #     inputTex(c("Age","Sex","Weight","Height"))
#' #    @
#' #
#' # Must have Age.tex, Sex.tex, Weight.tex and Height.tex in the
#' # same directory that the .Rnw file is.
#' #
#' # Output will look like:
#' #
#' # \input{Age.tex}
#' # \input{Sex.tex}
#' # \input{Weight.tex}
#' # \input{Height.tex}
#' # \clearpage
#' 
inputTex <- function(texName,
                     clearpage=TRUE,
                     multicols=FALSE,
                     numCols=2)
  {
    #set commonly used strings to variable names
    sl <- paste("\\")
    clpage <- paste(sl,"clearpage",sep="")
    tx <- "tex"
    #set defaults
    
    start <- paste("\\input",sep='')
    
    #create code
   
    if(!multicols)
      {
        for(i in 1:length(texName))
          {
            cat(paste(start,"{",texName[i],".tex} ",sep=""),sep="\n")
          }
      }
    else
      {
        begin <- paste(sl,"begin{multicols}",sep="")
        cat(paste(begin,"{",numCols,"}",sep=""), sep="\n")

        for(i in 1:length(texName))
          {
           cat(paste(start,"{",texName[i],"}",sep=""),sep="\n")
         }
        cat(paste(sl,"end{multicols}",sep=""),sep="\n")
      }
    if(clearpage)
          {
            cat(clpage,sep="\n")
          }
  }

