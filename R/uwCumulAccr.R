#' Cumulative Accrual Line Graphs
#'
#' Cumulative accrual line graphs for the total cohort or separated by
#' treatment level.  Also available is the ability to add projected accrual
#' lines for total cohort or separated by treatment level. Beneficial for DSMB
#' to judge if pre-determined accrual rates are being met.
#'
#'
#' @param accData Data frame which includes an accural date for each patient
#' @param dateName (string) Variable name in accData indicating the date of
#' accrual. Must be a variable with class type 'dates' from library(chron).
#' Function will try to coherce non-dates variables into dates of the format
#' 'm/d/y'.
#' @param trxName (string) Optional string giving the name of the variable in
#' accData that indicates treatment assignment. If used, then cumulative
#' accrual is given separately for each treatment level. If NULL then any
#' possible treatment levels are collapsed and treated as one large group
#' @param yLab (string) Y label of graphic
#' @param yLim (numeric) Limits of y axis of graphic. Default is the default of
#' plot
#' @param pTitle (string) Title of graphic
#' @param LegendCor Manually enter where the legend should be placed on the
#' graph.  Must be a set of coordinates of length 2 (X,Y).
#' @param startDate (string) Date in which the x-axis should start.  Generally
#' when accrual opened for the project.  Must be in m/d/y format. Defaults to
#' the minimum date in dateName.
#' @param currentDate (string) Date in which the x-axis should end. Generally
#' when report is being created.  Must be in m/d/y format.  Defaults to the
#' maximum date in dateName.
#' @param projectLine (list) List of length two with each element needing to be
#' numeric. PatsPerTRT is the number of patients expected to be accrued in each
#' treatment. Must be of length 1 or length equal to number of trxName levels.
#' If length is 1 then this indicates equal accrual in each arm. Order of
#' PatsPerTRT must match order of trxName levels. DaysPerTRT is the number of
#' days that enrollment will be open for each treatment arm.  Again if length
#' of 1 then this is equal accrual time for each arm.  If these numbers are
#' provided, then a projected accrual line will be added to the graphic either
#' for the total cohort or for the individual treatment arms
#' @param titleCex (numeric) Magnification of title size
#' @return a line graph
#' @author Scott J Hetzel MS. UW-Madison Department of Biostatistics and
#' Medical Informatics. Contributions from Patrick Lenon. Frontier Science and
#' Technology Research Foundation
#' @export
#' @examples
#'
#'
#' Dates <- c(
#'   "03/01/2008", "09/01/2007", "07/01/2007", "12/01/2006", "08/01/2007", "08/01/2007",
#'   "04/01/2007", "04/01/2007", "04/01/2007", "11/01/2006", "11/01/2006", "12/01/2006",
#'   "12/01/2006", "10/01/2006", "01/01/2007", "11/01/2006", "12/01/2006", "02/01/2007",
#'   "08/01/2007", "08/01/2006", "09/01/2006", "04/01/2007", "05/01/2007", "11/01/2006",
#'   "10/01/2006", "03/01/2007", "10/01/2007", "07/01/2007", "03/01/2007", "04/01/2006",
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
#' layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = T))
#'
#' uwCumulAccr(
#'   accData = data, dateName = "Date", trxName = NULL,
#'   yLab = "Cumulative Number of Subjects",
#'   yLim = NULL,
#'   pTitle = "Accural over Time for Total Cohort",
#'   projectLine = list(PatsPerTRT = as.numeric(), DaysPerTRT = as.numeric())
#' )
#' # Projected accrual of 32 per arm over 750 days
#' uwCumulAccr(
#'   accData = data, dateName = "Date", trxName = NULL,
#'   yLab = "Cumulative Number of Subjects",
#'   yLim = NULL, startDate = "01/01/06",
#'   pTitle = "Total Cohort with equal projected accrual",
#'   projectLine = list(PatsPerTRT = 64, DaysPerTRT = 750)
#' )
#'
#' uwCumulAccr(
#'   accData = data, dateName = "Date", trxName = "TRT",
#'   yLab = "Cumulative Number of Subjects",
#'   yLim = NULL,
#'   pTitle = "Accural over Time by Treatment Arm",
#'   projectLine = list(PatsPerTRT = as.numeric(), DaysPerTRT = as.numeric())
#' )
#' # Projected accrual of 30 in A and 27 in B over 700 days
#' uwCumulAccr(
#'   accData = data, dateName = "Date", trxName = "TRT",
#'   yLab = "Cumulative Number of Subjects",
#'   yLim = c(0, 70), LegendCor = c(25, 55),
#'   pTitle = "Accural over Time by Treatment Arm",
#'   projectLine = list(PatsPerTRT = c(30, 27), DaysPerTRT = 700)
#' )
uwCumulAccr <- function(accData, dateName, trxName = NULL, yLab = "Cumulative Number of Subjects", yLim = NULL,
                        pTitle = NULL, LegendCor = NULL, startDate = NULL, currentDate = NULL, projectLine = list(
                          PatsPerTRT = as.numeric(),
                          DaysPerTRT = as.numeric()
                        ), titleCex = 1) {
  require(chron)

  # Remove any lines of data that have missing dateName
  accData <- accData[!is.na(accData[[dateName]]), ]

  numTrx <- ifelse(is.null(trxName), 1, nlevels(accData[[trxName]]))

  # Make table of cume accrual by treatment

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

  # Get each date between startDate and currentDate
  timeseq <- seq.dates(startDate, currentDate, 1)

  # Create matrix of dates and treatment levels
  timetab <- matrix(rep(0, length(timeseq) * numTrx), ncol = length(timeseq), nrow = numTrx)
  colnames(timetab) <- as.character(timeseq)

  # Merge the dates into the time matrix
  if (is.null(trxName)) {
    datestab <- table(accData[[dateName]])
    timetab[match(names(datestab), colnames(timetab))] <- datestab
  } else {
    datestab <- table(accData[[trxName]], accData[[dateName]])
    timetab[, match(colnames(datestab), colnames(timetab))] <- datestab
    rownames(timetab) <- levels(accData[[trxName]])
  }

  # Then get cumulative accrual over time. This converts to treatment as columns no longer rows.
  cumtimetab <- apply(timetab, 1, cumsum)

  # Plot first trx, then use 'lines' to add any others

  # equiv. to c('44', '13', '1343', '73', '2262').
  lTypes <- seq(1:(numTrx + 1))

  if (is.null(yLim)) {
    yLim <- c(0, max(cumtimetab) * 1.1)
  }

  # Adjust axis label placements
  defmgp <- par("mgp")
  par(mgp = c(2, 0.5, 0))
  plot(cumtimetab[, 1], type = "s", main = "", ylab = yLab, xlab = "", ylim = yLim, axes = FALSE, lty = lTypes[1])

  # put box around title TAKEN FROM figureBox.R
  UperL <- ((par("usr")[4] - par("usr")[3]) / par("pin")[2]) / (par("mar")[3] / par("mai")[3])
  rect(par("usr")[1], par("usr")[4], par("usr")[2], par("usr")[4] + 1.2 * UperL, xpd = TRUE)
  # print title
  title(main = pTitle, line = 0.3, cex.main = titleCex, font.main = 1)

  # plot remainder of trx's
  if (numTrx > 1) {
    for (i in 2:numTrx) {
      # change line type with each line also
      lines(cumtimetab[, i], type = "l", lty = lTypes[i])
    }
  }

  axis(2, at = axTicks(2), las = 2, cex.axis = 0.8, tcl = -0.2)

  vDates <- seq.dates("1/1/1900", currentDate, "months")
  vDates <- vDates[vDates >= startDate]
  xLabel <- as.character(vDates)
  atX <- vDates - startDate

  axis(1, labels = xLabel, tcl = -0.2, at = atX, cex.axis = 0.8)
  par(mgp = defmgp)

  # Set Legend Text
  if (is.null(trxName)) {
    lgText <- paste("Total Subjects =", cumtimetab[length(cumtimetab), ], sep = "")
  } else {
    lgText <- paste(colnames(cumtimetab), " n=", cumtimetab[nrow(cumtimetab), ], sep = "")
  }

  # Project Accrual Line
  if (!is.list(projectLine) | length(projectLine) != 2) {
    stop("projectLine must be a list of length 2")
  }

  projPat <- projectLine[[1]]
  projTime <- projectLine[[2]]

  if (is.null(LegendCor)) {
    xLeg <- length(cumtimetab[1, ]) * 0.1
    yLeg <- max(cumtimetab) * 0.9
  } else {
    if (length(LegendCor) != 2) {
      stop("LegendCor must be null or a set of coordinates of length 2")
    }
    xLeg <- LegendCor[1]
    yLeg <- LegendCor[2]
  }

  if (length(projPat) != 0 & length(projTime) != 0) {
    if (length(projPat) != 1 & length(projPat) != numTrx) {
      stop("PatsPerTRT must either be length 1 or length of number of levels in trxName")
    }

    if (length(projTime) != 1 & length(projTime) != numTrx) {
      stop("DaysPerTRT must either be length 1 or length of number of levels in trxName")
    }

    if (!is.numeric(projTime) | !is.numeric(projPat)) {
      stop("Both parts of projectLine must be numeric")
    }

    if (length(projTime) != length(projPat)) {
      projTime <- rep(projTime, length = numTrx)
      projPat <- rep(projPat, length = numTrx)
    }
    # Test if all numbers match (i.e. Even Accrual per Arm)
    if (sum(projTime[1] == projTime) == length(projTime) & sum(projPat[1] == projPat) == length(projPat)) {
      projTime <- projTime[1]
      projPat <- projPat[1]
    }

    if (length(projTime) == 1) {
      lines(x = c(0, projTime), y = c(0, projPat), lty = rev(lTypes)[1], col = "gray60")
      # need a legend here
      lg <- legend(
        x = xLeg, y = yLeg, legend = c(lgText, "All Arm Projection"), lty = lTypes, cex = 0.7,
        col = c(rep("black", numTrx), "gray60")
      )
    } else {
      for (i in 1:length(projTime)) {
        lines(x = c(0, projTime[i]), y = c(0, projPat[i]), lty = lTypes[i], col = "gray60")
      }
      # need a legend here
      lg <- legend(x = xLeg, y = yLeg, legend = c(lgText, paste(colnames(cumtimetab), " projection",
        sep = ""
      )), lty = rep(lTypes[1:numTrx], 2), cex = 0.7, col = rep(
        c("black", "gray60"),
        c(2, 2)
      ))
    }
  } else {
    lg <- legend(x = xLeg, y = yLeg, legend = c(lgText), lty = lTypes[1:numTrx], cex = 0.7)
  }


  box()
}
