#' Creation of LaTex Code to start the .Rnw Sweave file
#'
#' Creation of .Rnw Sweave file for LaTeX and R code compiling and ultimately
#' PDF report creation.  Created to replace repTex
#'
#' This was created as a convenience for new reports.  No longer copy and paste
#' and edit LaTeX code from previous projects for a new project.  Do not run
#' this function twice for the same project.  If this file already exists it
#' will be erased first and recreated therefore work added to the file will be
#' lost.
#'
#' @param filepath String giving the directory path to the directory where the
#' .Rnw file will be created
#' @param filename String giving the name of the file
#' @param stamp String giving the background stamp that will be on every page
#' of the LaTeX report
#' @param title String giving the title for the report
#' @param chead String giving the center header that will be on the top of
#' every page of the report
#' @param cfoot String giving the center foot that will be on the bottom of
#' every page of the report
#' @return Output is a .Rnw file.  File is started with standard LaTeX preamble
#' that should be used in creating a LaTeX report.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso Sweave()
#' @export
#' @importFrom utils file_test
#' @examples
#'
#'
#' # This will create a report.Rnw file in the working Rdirectory
#' beginSweaveFile()
beginSweaveFile <- function(filepath = getwd(), filename = "report", stamp = "Confidential", title = "Fix Title",
                            chead = "", cfoot = "") {

  # Check if file already exists.  If so delete and start new

  if (file_test("-f", paste(filepath, "/", filename, ".Rnw", sep = ""))) {
    file.remove(paste(filepath, "/", filename, ".Rnw", sep = ""))
  }

  # Set commonly used strings to variable names
  sl <- paste("\\")
  begin <- paste(sl, "begin", sep = "")
  clpage <- paste(sl, "clearpage", sep = "")
  sect <- paste(sl, "section{\\Large ", sep = "")
  inp <- paste(sl, "input", sep = "")
  usepk <- paste(sl, "usepackage", sep = "")
  newcom <- paste(sl, "newcommand{", sep = "")
  setln <- paste(sl, "setlength{", sep = "")
  end <- paste(sl, "end", sep = "")

  # create file name
  file <- paste(filepath, "/", filename, ".Rnw", sep = "")

  # create code
  cat(c(paste(sl, "documentclass[11pt]{article}", sep = ""), paste(sl, "SweaveOpts{echo=FALSE, fig=TRUE, pdf=FALSE, eps=TRUE, width=7, height=9}",
    sep = ""
  ), paste(sl, "makeatletter", sep = "")), file = file, sep = "\n")

  cat(c(
    paste(usepk, "{graphics}", sep = ""), paste(usepk, "{graphicx}", sep = ""), paste(usepk, "{psfig}",
      sep = ""
    ), paste(usepk, "{fancyhdr}", sep = ""), paste(usepk, "{setspace}", sep = ""), paste(usepk,
      "{times}",
      sep = ""
    ), paste(usepk, "{rotating}", sep = ""), paste(usepk, "[small,normal,bf,up]{caption2}",
      sep = ""
    ), paste(usepk, "{makeidx,showidx,longtable}", sep = ""), paste(usepk, "{multicol}", sep = ""),
    paste(usepk, "{multirow}", sep = ""), paste(usepk, "{portland}", sep = ""), paste(usepk, "{url}",
      sep = ""
    ), paste(sl, "renewcommand{", sl, "captionfont}{", sl, "small}", sep = "")
  ),
  file = file,
  sep = "\n", append = TRUE
  )

  cat(c(
    paste(setln, sl, "textheight}{9.0in}", sep = ""), paste(setln, sl, "textwidth}{7.2in}", sep = ""),
    paste(setln, sl, "topmargin}{-0.25in}", sep = ""), paste(setln, sl, "headheight}{0.5in}", sep = ""),
    paste(setln, sl, "oddsidemargin}{-0.2in}", sep = ""), paste(setln, sl, "headsep}{0.25in}", sep = ""),
    paste(setln, sl, "topskip}{0in}", sep = "")
  ), file = file, sep = "\n", append = TRUE)

  cat(c(paste(newcom, sl, "figstart}{", begin, "{figure}[!hbp]", sep = ""), paste(begin, "{center}}",
    sep = ""
  ), paste(newcom, sl, "figend}[2]{", sep = ""), paste(end, "{center}", sep = ""), paste(sl,
    "caption{", sl, "small #1}",
    sep = ""
  ), paste(sl, "label{#2}", sep = ""), paste(end, "{figure}}",
    sep = ""
  )), file = file, sep = "\n", append = TRUE)

  cat(c(paste(newcom, sl, "defHt}{", sep = ""), paste(sl, "setkeys{Gin}{height=8.7in}", sep = ""), paste(sl,
    "setkeys{Gin}{width=7.5in}}",
    sep = ""
  )), file = file, sep = "\n", append = TRUE)

  cat(c(paste(newcom, sl, "rptdate}{", sl, "today}", sep = ""), paste(sl, "renewcommand{", sl, "figurename}{Fig.}",
    sep = ""
  ), paste(sl, "renewcommand{", sl, "tablename}{Tab.}", sep = ""), paste(sl, "pagestyle{fancy}",
    sep = ""
  )), file = file, sep = "\n", append = TRUE)

  cat(paste(sl, "makeindex", sep = ""), file = file, sep = "\n", append = TRUE)

  cat(c(paste(sl, "newenvironment{tablehere}", sep = ""), paste("{", sl, "def", sl, "@captype{table}}",
    sep = ""
  ), paste("{}", sep = ""), paste(sl, "newenvironment{figurehere}", sep = ""), paste("{",
    sl, "def", sl, "@captype{figure}}",
    sep = ""
  ), paste("{}", sep = ""), paste(sl, "makeatother",
    sep = ""
  )), file = file, sep = "\n", append = TRUE)

  cat(paste(begin, "{document}", sep = ""), file = file, sep = "\n", append = TRUE)

  cat(c(paste(newcom, sl, "stamp}[1]{", sl, "special{!userdict begin", sep = ""), paste("/bop-hook{gsave 200 150 translate 45 rotate",
    sep = ""
  ), paste("/Times-Roman findfont 80 scalefont", sep = ""), paste("setfont 0 0 moveto 0.92 setgray (#1) show grestore}def end}}",
    sep = ""
  ), paste(sl, "stamp{", stamp, "}", sep = "")), file = file, sep = "\n", append = TRUE)

  cat(c(paste(sl, "thispagestyle{empty}", sep = ""), paste(newcom, sl, "HRule}{", sl, "rule{", sl, "linewidth}{1mm}}",
    sep = ""
  ), paste(setln, sl, "parindent}{0mm}", sep = ""), paste(setln, sl, "parskip}{0mm}", sep = "")),
  file = file, sep = "\n", append = TRUE
  )

  cat(c(paste(sl, "vspace*{", sl, "stretch{0.4}}", sep = ""), paste(sl, "HRule", sep = ""), paste(begin,
    "{center}{",
    sep = ""
  ), paste(sl, "Large Prepared as", " ", sl, sl, sep = ""), paste(sl, "Huge Data Analysis for",
    " ", sl, sl,
    sep = ""
  ), paste(sl, "Large ", title, " ", sl, sl, sep = ""), paste("}"), paste(sl,
    "rptdate",
    sep = ""
  ), paste(end, "{center}", sep = "")), file = file, sep = "\n", append = TRUE)

  cat(c(paste(sl, "HRule", sep = ""), paste(sl, "vspace*{", sl, "stretch{2}}", sep = ""), paste(begin,
    "{center}",
    sep = ""
  ), paste(sl, "large ", sl, "textsc{Department of Biostatistics and Medical Informatics",
    " ", sl, sl,
    sep = ""
  ), paste("University of Wisconsin -- Madison}", sep = ""), paste(end, "{center}",
    sep = ""
  )), file = file, sep = "\n", append = TRUE)

  cat(c(
    clpage, paste(sl, "lhead{", sl, "rptdate}", sep = ""), paste(sl, "chead{", chead, "}", sep = ""),
    paste(sl, "rhead{Page ", sl, "thepage}", sep = ""), paste(sl, "cfoot{", cfoot, "}", sep = ""),
    paste(sl, "tableofcontents", sep = ""), paste(sl, "listoffigures", sep = ""), paste(sl, "listoftables",
      sep = ""
    ), paste(sl, "newpage", sep = "")
  ), file = file, sep = "\n", append = TRUE)

  cat(c(
    "", "", "", paste("% Edit this file and place the LaTeX and R code in this area according to how a Sweave document should be laid out.  The function used to produce this file should not be run again for this file because whatever work you have done will be erased and you will be back to the beginning.  Best Wishes!"),
    "", "", ""
  ), file = file, sep = "\n", append = TRUE)


  cat(paste(end, "{document}", sep = ""), file = file, sep = "\n", append = TRUE)
}
