#' Bar plot of frequency of accural
#'
#' Bar plot of frequency of accural by days, months, or years.  Mostly for DSMB
#' use to assess accural
#'
#'
#' @param accData Data frame which includes an accural date for each patient
#' @param dateName (string) Variable name in accData indicating the date of
#' accrual. Must be a variable with class type 'dates' from library(chron). The
#' function will try to coherece a non-dates variable into a m/d/y formatted
#' dates class
#' @param trxName (string) Optional string giving the name of the variable in
#' accData that indicates treatment assignment. If used, then accrual for each
#' time point is given separately for each treatment level. If NULL then any
#' possible treatment levels are collapsed and treated as one large group
#' @param interval (string) Indicator of which time interval is to be
#' presented.  Default is 'months'.  1 letter abbreviation is sufficient
#' @param startDate (date) Specify the start date of accrual.  Defaults to the
#' minimum date of dateName
#' @param currentDate (date) Specify the current date.  Defaults to the maximum
#' date of dateName
#' @param pTitle (string) Title of graphic
#' @param yLab (string) Y label of graphic
#' @param xLab (string) X label of graphic
#' @param yLim (numeric) Limits of y axis of graphic. Default is the default of
#' barplot
#' @param pHoriz (logic) horiz attribute for barplot().  TRUE prints bars
#' horizontally
#' @param pBeside (logic) beside attribute for barplot().  FALSE prints stacked
#' bars, TRUE prints side-by-side bars
#' @param linedbars (logic) TRUE for lined bars, FALSE for solid colored bars
#' @param printBarVals (logic) TRUE for the frequency of each bar to be plot on
#' top of the bars.  0 values are suppressed from being printed
#' @param Legend (logic) TRUE for Legend to be printed. Only feasible if
#' trxName is not NULL.
#' @param LegendLoc (string) Strings allotted in legend() to be used as a
#' location for the legend.  See help(legend) for further explanation.
#' @param LegendCex (numeric) Multiplicative factor of printing size of legend
#' @param cex.names (numeric) Multiplicative factor of printing size of legend
#' axis label
#' @param titleCex (numeric) Multiplicative factor of printing size of title
#' @param barNamesLas (0,1,2 or 3) Option for axis labels to be horizontal or
#' veritcal
#' @param barNamesSeq (integer) Number indicating which time labels to print.
#' This number is given as the 'by' attribute to seq().  So every barNameSeq
#' would be printed.  Default is 1, which will print all time labels.  Mostly
#' helpful when interval='d'
#' @param barNamesAngle (integer) Number between 0 and 360 that gives the angle
#' in which the axis names will be printed.
#' @return invisible
#' @author Scott J Hetzel MS.  UW-Madison Department of Biostatistics and
#' Medical Informatics. Contributions from Patrick Lenon. Frontier Science and
#' Technology Research Foundation
#' @seealso uwBars
#' @export
#' @examples
#'
#'
#' Dates <- c(
#'   "03/01/2008", "09/01/2007", "07/01/2007", "12/01/2006", "08/01/2007", "08/01/2007",
#'   "04/01/2007", "04/01/2007", "04/01/2007", "11/01/2006", "11/01/2006", "12/01/2006",
#'   "12/01/2006", "10/01/2006", "01/01/2007", "11/01/2006", "12/01/2006", "02/01/2007",
#'   "08/01/2007", "08/01/2006", "09/01/2006", "04/01/2007", "05/01/2007", "11/01/2006",
#'   "10/01/2006", "03/01/2007", "10/01/2007", "07/01/2007", "03/01/2007", "05/01/2006",
#'   "05/01/2006", "11/01/2006", "12/01/2006", "02/01/2007", "02/01/2007", "02/01/2007",
#'   "04/01/2007", "05/01/2007", "07/01/2007", "08/01/2007", "11/01/2007", "10/01/2007",
#'   "09/01/2007", "10/01/2007", "05/01/2007", "01/01/2008", "12/01/2006", "02/01/2007",
#'   "03/01/2007", "04/01/2007", "04/01/2007", "05/01/2007", "07/01/2007", "02/01/2007",
#'   "07/01/2007", "10/01/2007", "01/01/2008"
#' )
#'
#' library(chron)
#' Date <- chron(Dates, format = c(dates = "m/d/y"))
#' TRT <- rep(c("A", "B"), c(30, 27))
#'
#' data <- data.frame(Date, TRT)
#'
#' layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
#' uwAccrualBars(
#'   accData = data, dateName = "Date", trxName = NULL, pTitle = "Accural",
#'   yLab = "Months", xLab = "Number of Patients",
#'   interval = "m", yLim = NULL, pHoriz = TRUE, pBeside = TRUE,
#'   linedbars = FALSE, Legend = TRUE, LegendLoc = "right",
#'   LegendCex = 0.5, cex.names = 0.7, barNamesLas = 1
#' )
#'
#' uwAccrualBars(
#'   accData = data, dateName = "Date", trxName = NULL, pTitle = "Test",
#'   yLab = "Number of Patients", xLab = "Day", startDate = "01/01/2005", currentDate = "12/31/2010",
#'   interval = "d", yLim = NULL, pHoriz = FALSE, pBeside = TRUE,
#'   linedbars = TRUE, Legend = FALSE, LegendLoc = "topleft",
#'   LegendCex = 0.6, cex.names = 0.8, barNamesSeq = 40
#' )
#'
#' uwAccrualBars(
#'   accData = data, dateName = "Date", trxName = NULL, pTitle = "Bars",
#'   yLab = "Months", xLab = "Number of Subjects", printBarVals = FALSE,
#'   interval = "y", yLim = NULL, pHoriz = TRUE, pBeside = FALSE,
#'   linedbars = TRUE, Legend = FALSE, LegendLoc = "bottomleft",
#'   LegendCex = 0.7, cex.names = 0.9
#' )
#'
#' uwAccrualBars(
#'   accData = data, dateName = "Date", trxName = NULL,
#'   interval = "m", yLim = NULL, pHoriz = FALSE, pBeside = FALSE,
#'   linedbars = FALSE, Legend = TRUE
#' )
uwAccrualBars <- function(accData, dateName, trxName = NULL, interval = c("months", "days", "years"), startDate = NULL,
                          currentDate = NULL, pTitle = "Accrual Over Time", yLab = NULL, xLab = NULL, yLim = NULL, pHoriz = FALSE,
                          pBeside = TRUE, linedbars = FALSE, printBarVals = TRUE, Legend = FALSE, LegendLoc = "topright", LegendCex = 0.8,
                          cex.names = 1, titleCex = 1, barNamesLas = 1, barNamesSeq = 1, barNamesAngle = 0) {
  require(chron)
  # Remove any lines of data that have missing dateName
  accData <- accData[!is.na(accData[[dateName]]), ]
  # If trxName is NULL then Legend must be FALSE
  if (is.null(trxName) & Legend) {
    print("if trxName is NULL Legend must be FALSE")
    Legend <- FALSE
  }
  # Try to set default of interval to be months, doesn't cover all situations
  interval <- ifelse(length(interval) >= 1, interval[1], "months")

  if (!(interval %in% c("d", "m", "y") | interval %in% c("days", "months", "years") | interval %in% c(
    "day",
    "month", "year"
  ))) {
    print("interval is not a valid string defaults to months")
    interval <- "months"
  }
  # reduce interval to 1 character for ease of checking
  interval <- substr(interval, 1, 1)

  # check to make sure dateName is a date object
  if (!("dates" %in% class(accData[[dateName]]))) {
    warning("Cohercing accData[[dateName]] to a dates class of m/d/y")
    accData[[dateName]] <- chron(as.character(accData[[dateName]]), format = c(dates = "m/d/y"))
  }

  if (is.null(startDate)) {
    startDate <- min(accData[[dateName]])
  } else if (startDate > min(accData[[dateName]])) {
    stop(paste("startDate is greater than min Date in accData[[dateName]]:", min(accData[[dateName]]),
      sep = ""
    ))
  }
  if (is.null(currentDate)) {
    currentDate <- max(accData[[dateName]])
  } else if (currentDate < max(accData[[dateName]])) {
    stop(paste("curentDate is less than max Date in accData[[dateName]]:", max(accData[[dateName]]),
      sep = ""
    ))
  }

  # create time table for plotting in barplot
  if (interval == "d") {
    # timeseq <- seq.dates(min(accData[[dateName]]),max(accData[[dateName]]),1)
    timeseq <- seq.dates(startDate, currentDate, 1)
    if (is.null(trxName)) {
      # timetab <- table(cut(accData[[dateName]],'days'))
      timetab <- table(cut(c(accData[[dateName]], startDate, currentDate), "days"))
      names(timetab) <- as.character(timeseq)
      # Subtract out the entry of artificial dates startDate and currentDate
      timetab[1] <- timetab[1] - 1
      timetab[length(timetab)] <- timetab[length(timetab)] - 1
    } else {
      # timetab <- table(accData[[trxName]],cut(accData[[dateName]],'days'))
      timetab <- table(c(accData[[trxName]], NA, NA), cut(
        c(accData[[dateName]], startDate, currentDate),
        "days"
      ))
      colnames(timetab) <- as.character(timeseq)
      rownames(timetab) <- levels(accData[[trxName]])
    }
  } else if (interval == "m") {
    if (is.null(trxName)) {
      # timetab <- table(cut(accData[[dateName]],'months'))
      timetab <- table(cut(c(accData[[dateName]], startDate, currentDate), "months"))
      timetab[1] <- timetab[1] - 1
      timetab[length(timetab)] <- timetab[length(timetab)] - 1
    } else {
      timetab <- table(c(accData[[trxName]], NA, NA), cut(
        c(accData[[dateName]], startDate, currentDate),
        "months"
      ))
      rownames(timetab) <- levels(accData[[trxName]])
    }
  } else {
    if (is.null(trxName)) {
      # timetab <- table(cut(accData[[dateName]],'years'))
      timetab <- table(cut(c(accData[[dateName]], startDate, currentDate), "years"))
      timetab[1] <- timetab[1] - 1
      timetab[length(timetab)] <- timetab[length(timetab)] - 1
    } else {
      timetab <- table(c(accData[[trxName]], NA, NA), cut(
        c(accData[[dateName]], startDate, currentDate),
        "years"
      ))
      rownames(timetab) <- levels(accData[[trxName]])
    }
  }

  if (is.null(trxName)) {
    pBeside <- TRUE
  }

  if (pHoriz) {
    mgp.y <- c(3, 0.5, 0)
    mgp.x <- c(1.5, 0.5, 0)
  } else {
    mgp.y <- c(1.5, 0.5, 0)
    mgp.x <- c(1.5, 0.5, 0)
  }

  uwb <- uwBar(timetab,
    pBeside = pBeside, pHoriz = pHoriz, lined = linedbars, axLim = yLim, yLab = yLab,
    xLab = xLab, printBarVals = printBarVals, Legend = Legend, LegendLoc = LegendLoc, LegendCex = LegendCex,
    mgp.y = mgp.y, mgp.x = mgp.x, cex.names = cex.names, barNamesLas = barNamesLas, barNamesSeq = barNamesSeq,
    barNamesAngle = barNamesAngle, digits = 0
  )

  # put box around title TAKEN FROM figureBox.R
  UperL <- ((par("usr")[4] - par("usr")[3]) / par("pin")[2]) / (par("mar")[3] / par("mai")[3])
  rect(par("usr")[1], par("usr")[4], par("usr")[2], par("usr")[4] + 1.2 * UperL, xpd = TRUE)
  # print title
  title(main = pTitle, line = 0.3, cex.main = titleCex, font.main = 1)
}
