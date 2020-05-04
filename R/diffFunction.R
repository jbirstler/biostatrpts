#' Function for finding differences between different factor levels for the
#' same patient for either numeric or ordinal variables
#'
#' This function is meant for finding differences between different factor
#' levels for the same patient for either numeric or ordinal variables.  It can
#' be used inside the AEfactorized() and uwLeveledBoxPlot by setting
#' delta=TRUE, or by itself
#'
#'
#' @param Diffdata Name of data set in R
#' @param variable String of variable name of interest in Diffdata
#' @param TrxName String of treatment variable name in Diffdata
#' @param VisitName String of variable name for time points
#' @param Baseline String of level name in VisitName to be used as baseline
#' @param ptID String of variable name for Identification Numbers
#' @param deltaPct Logical. TRUE gives percentage change from baseline
#' @return A data frame is returned with difference column, time column,
#' treatment column, and ID column.
#'
#' Differences are set so the baseline value is being subtracted from the
#' non-baseline value.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.  Assistance from Frontier Science
#' and Technology Reseach Foundation, Pat Lenon and Zekai Otles.
#' @seealso AEfactorized(), uwLeveledBoxPlot()
#' @export
#' @examples
#'
#' # Example
#'
#' ID <- rep(letters[1:20], 3)
#' TRT <- rep(c("A", "B"), 30)
#' Time <- c(rep("Baseline", 20), rep("Week 5", 20), rep("Week 10", 20))
#' Time <- ordered(Time, c("Baseline", "Week 5", "Week 10"))
#' AE <- sample(c(1, 2, 3, 4), 60, replace = TRUE)
#'
#' data2 <- data.frame(ID, TRT, Time, AE)
#'
#' diffFunction(
#'   Diffdata = data2,
#'   variable = "AE",
#'   TrxName = "TRT",
#'   VisitName = "Time",
#'   Baseline = "Baseline",
#'   ptID = "ID",
#'   deltaPct = TRUE
#' )
diffFunction <- function(Diffdata, variable, TrxName, VisitName, Baseline, ptID, deltaPct = FALSE) {

  # Narrow data down to just the variable to be summarized and necessary other variables
  data <- data.frame(
    variable = Diffdata[[variable]], Trt = Diffdata[[TrxName]], VisitName = Diffdata[[VisitName]],
    ptID = Diffdata[[ptID]]
  )

  if (is.numeric(data$Trt)) {
    data$Trt <- as.factor(data$Trt)
  }
  # Get Time levels
  VisitNameLevels <- levels(data$VisitName)

  # Remove any patients that don't have a baseline value
  basepts <- data$ptID[data$VisitName == Baseline]

  if (nrow(subset(data, subset = data$ptID %in% basepts)) != nrow(data)) {
    nobasepts <- unique(data$ptID[!(data$ptID %in% basepts)])
    warning(paste("These patients", paste(nobasepts, collapse = ", "), "do not have a baseline measure and will be removed"))
    data <- subset(data, subset = data$ptID %in% basepts)
  }

  # Check that each patient has a unique baseline measure
  if (length(basepts) != length(unique(basepts))) {
    multibases <- names(summary(as.factor(basepts)))[summary(as.factor(basepts)) > 1]
    warning(paste("These patients", paste(multibases, collapse = ", "), "had more than 1 baseline measure"))
    stop("Program was stopped due to patients with multiple baseline measures. See Warnings")
  }

  # Create a data frame of the measures by time point

  dframe <- data.frame(
    ID = sort(basepts), TRT = data$Trt[data$VisitName == Baseline][order(basepts)],
    Bline = data$variable[data$VisitName == Baseline][order(basepts)]
  )

  for (i in 1:length(VisitNameLevels)) {
    if (VisitNameLevels[i] != Baseline) {
      lev <- VisitNameLevels[i]
      tempvar <- data$variable[data$VisitName == lev]
      dframe <- cbind(dframe, temp = rep(NA, nrow(dframe)))
      dframe$temp <- tempvar[match(dframe$ID, data$ptID[data$VisitName == lev])]
      colnames(dframe)[ncol(dframe)] <- lev
    }
  }

  if (deltaPct) {
    # Check for any 0s in Basline
    if (sum(dframe$Bline == 0, na.rm = TRUE) > 0) {
      stop("There are 0s in the baseline measure. Percent change is not feasible")
    }
  }
  # Reset colname dframe$Baseline to level given in Baseline
  colnames(dframe)[colnames(dframe) == "Bline"] <- Baseline


  diffCol <- c()
  timeCol <- c()
  diffTRT <- c()
  idCol <- c()
  for (i in 3:ncol(dframe)) {
    tempdiff <- dframe[, i] - dframe[, 3]
    if (is.numeric(dframe$ID)) {
      idCol <- c(idCol, dframe$ID[!is.na(tempdiff)])
    } else if (is.factor(dframe$ID)) {
      idCol <- c(idCol, levels(dframe$ID)[dframe$ID[!is.na(tempdiff)]])
    }
    diffTRT <- c(diffTRT, levels(dframe$TRT)[dframe$TRT[!is.na(tempdiff)]])
    timeCol <- c(timeCol, rep(colnames(dframe)[i], length(na.omit(tempdiff))))

    if (deltaPct) {
      diffCol <- c(diffCol, round(na.omit(tempdiff / dframe[, 3]), 4))
    } else {
      diffCol <- c(diffCol, na.omit(tempdiff))
    }
  }
  data2 <- data.frame(idCol, diffTRT, timeCol, diffCol)
  data2$timeCol <- ordered(data2$timeCol, VisitNameLevels)
  return(data2)
}
