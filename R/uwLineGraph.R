#' Line Graph of Means or Medians
#'
#' Line Graph of Means or Medians over multiple factor levels, most often time
#' points
#'
#'
#' @param data Data frame containing the necessary variables described below
#' @param metricName (string) Name of numeric column in data
#' @param trxName (string) Name of Treatment column in data.  There will be
#' separate lines for each treatment level
#' @param factorName (sting) Name of factor column in data.  There will be
#' plotted points of the mean and median of each treatment at each factor
#' level.  Factor levels are on the x-axis and it is recommend that it is an
#' ordered factor
#' @param LatexFileName (string) Giving the folder and file(ending in .tex) for
#' the LaTeX table to be saved., length=2 if pairDiffTab=TRUE
#' @param plotMean (logical) TRUE plots mean values, FALSE plots median values
#' @param pch (vector) Numeric specifying which plotting points should be used
#' to represent different treatment levels.  Default is
#' c(19,21,23,24,25,7,8,17,0,15)[1:nlevels(trxName)]
#' @param colScale (logic or vector) Default is FALSE which plots the lines and
#' points on a gray scale.  TRUE plots the lines and points on a color scale
#' like so: c('black','red','blue','forestgreen',
#' 'gold2','violet','brown','chartreuse','lightgreen',
#' 'magenta')[1:nlevels(trxName)], or you can put in your own set of colors;
#' length should match nlevels of trxName
#' @param yLim Limits of y axis, default makes sure there is a tick mark above
#' the max value and below the min value
#' @param yLab (string) y-axis Label
#' @param xLab (string) x-axis Label
#' @param pTitle (string) Plot Title
#' @param refLines (logical) TRUE plots dotted lines across the plot at each
#' y-axis tick, FALSE does not
#' @param Legend (logical) Show Legend in plot or not
#' @param LegendLoc (string) Placement of Legend according legend()
#' @param LegendCex (numeric) Magnification factor of the legend size
#' @param printPVals (logical) TRUE corresponds to p-values placed in Latex
#' table
#' @param pTest (string) Only if printPVals=TRUE: Which test should be
#' conducted out of wilcoxon, t.test, anova, or kruskal.  Starting letter will
#' suffice
#' @param pExclude (string) Only if printPVals=TURE: List the treatment levels
#' (other than control level) that you do not want to report p-values for.
#' Does not apply when pTest is anova or kruskal
#' @param trxControl (string) Only if printPVals=TRUE: List which level of
#' trxName is considered the control group
#' @param pairDiffTab (logical) TRUE if paired differences want to be examined
#' because factorName indicates time points of repeated measures.
#' @param ptIDname (string) Only if pairDiffTab=TRUE: Column name in data that
#' indicates patient ID's
#' @param BaseLevel (sring) Only if pairDiffTab=TRUE: Level in factorName that
#' should be considered the baseline level for differencing
#' @param numDec (numeric) Number specifying how many decimals the summary
#' statistics should be rounded too.
#' @param rowLines (list of two logical vectors) Used in specifying horizontal
#' lines in the LaTeX tables.  First list spot is for regular table, second is
#' for difference table.  If pairDiffTab=FALSE, then this needs not be a list.
#' See ?uwLatex
#' @param caption (list of two logical vectors) Used in specifying the captions
#' in the LaTeX tables. First list spot is for regular table, second is for
#' difference table.  If pairDiffTab=FALSE, then this needs not be a list.  See
#' ?uwLatex
#' @param firstCol.name (string) Specifies the first column name in the LaTeX
#' tables.  Defaults to factorName
#' @param Ropen (logical) TRUE collapses across treatment levels and plots mean
#' or median for all patients
#' @param ... Any other arguments that can be passed to uwLatex
#' @export
#' @importFrom gdata drop.levels
#' @examples
#'
#'
#' TRT <- rep(rep(c("A", "B", "C"), c(20, 20, 20)), 4)
#'
#' ptID <- rep(1:60, 4)
#' ptID <- factor(ptID)
#'
#' numVar <- c(
#'   rnorm(20, 0, 1), rnorm(20, 1, 1), rnorm(20, 2, 1), rnorm(20, 3, 1),
#'   rnorm(20, 3, 1), rnorm(20, 10, 1), rnorm(20, 2, 3), rnorm(20, 0, 1),
#'   rnorm(20, 6, 1), rnorm(20, 1, 3), rnorm(20, 2, 5), rnorm(20, 3, 1)
#' )
#'
#' timeVar <- rep(c("Baseline", "3 Months", "6 Months", "12 Months"), c(60, 60, 60, 60))
#' timeVar <- ordered(
#'   timeVar,
#'   c("Baseline", "3 Months", "6 Months", "12 Months")
#' )
#'
#' df <- data.frame(TRT, ptID, numVar, timeVar)
#'
#'
#' uwLineGraph(
#'   data = df,
#'   metricName = "numVar",
#'   trxName = "TRT",
#'   factorName = "timeVar",
#'   LatexFileName = NULL,
#'   plotMean = TRUE,
#'   pch = NULL,
#'   colScale = TRUE,
#'   yLim = NULL,
#'   yLab = "",
#'   xLab = "",
#'   pTitle = "Example of uwLineGraph",
#'   refLines = TRUE,
#'   Legend = TRUE,
#'   LegendLoc = "topright",
#'   printPVals = TRUE,
#'   pTest = "t",
#'   pExclude = NULL,
#'   trxControl = "A",
#'   pairDiffTab = TRUE,
#'   ptIDname = "ptID",
#'   BaseLevel = "Baseline",
#'   numDec = 2,
#'   rowLines = list(c(FALSE), c(FALSE)),
#'   caption = list(c(NULL), c(NULL)),
#'   firstCol.name = "Time",
#'   Ropen = FALSE
#' )
uwLineGraph <- function(data, metricName, trxName, factorName, LatexFileName = NULL, plotMean = FALSE,
                        pch = NULL, colScale = FALSE, yLim = NULL, yLab = "", xLab = "", pTitle = NULL, refLines = TRUE, Legend = TRUE,
                        LegendLoc = "topright", LegendCex = 0.7, printPVals = TRUE, pTest = c("wilcox", "t", "kruskal", "anova"),
                        pExclude = NULL, trxControl = NULL, pairDiffTab = FALSE, ptIDname = NULL, BaseLevel = NULL, numDec = 2,
                        rowLines = list(c(FALSE), c(FALSE)), caption = list(c(NULL), c(NULL)), firstCol.name = NULL, Ropen = FALSE,
                        ...) {
  # strip data set to only relevent variables
  if (pairDiffTab) {
    dat <- subset(data, select = c(metricName, trxName, factorName, ptIDname))
  } else {
    dat <- subset(data, select = c(metricName, trxName, factorName))
  }
  # remove any missing values in any of the variables
  dat <- dat[apply(is.na(dat), 1, sum) == 0, ]
  dat[[trxName]] <- drop.levels(dat[[trxName]], reorder = FALSE)
  dat[[factorName]] <- drop.levels(dat[[factorName]], reorder = FALSE)

  if (Ropen) {
    # essentially remove trx as a factor
    levels(dat[[trxName]]) <- rep("", nlevels(dat[[trxName]]))
    printPVals <- FALSE
  }
  trxLevs <- levels(dat[[trxName]])
  NtrxLevs <- nlevels(dat[[trxName]])
  factLevs <- levels(dat[[factorName]])
  NfactLevs <- nlevels(dat[[factorName]])

  meanVals <- tapply(dat[[metricName]], dat[[trxName]]:dat[[factorName]], mean)
  medVals <- tapply(dat[[metricName]], dat[[trxName]]:dat[[factorName]], median)

  # set which points to plot
  if (plotMean) {
    plotPts <- meanVals
  } else {
    plotPts <- medVals
  }

  # set pch for distinguishing points
  if (is.null(pch)) {
    pchPts <- c(19, 21, 23, 24, 25, 7, 8, 17, 0, 15)[1:NtrxLevs]
  } else {
    if (length(pch) != NtrxLevs) {
      warning("length(pch)!=Number of Trt Levels, pch replicated to match number of levels")
      pchPts <- rep(pch, length = NtrxLevs)
    } else {
      pchPts <- pch
    }
  }
  # set color, on gray scale, or colors
  if (is.logical(colScale)) {
    if (!colScale) {
      color <- paste("gray", ceiling(seq(from = 10, to = 80, length = NtrxLevs)))
    } else {
      color <- c(
        "black", "red", "blue", "forestgreen", "gold2", "violet", "brown", "chartreuse",
        "lightgreen", "magenta"
      )[1:NtrxLevs]
    }
  } else if (is.character(colScale)) {
    if (length(colScale) != NtrxLevs) {
      warning("length(colScale)!=Number of Trt Levels, colScale replicated to match number of levels")
      color <- rep(colScale, length(NtrxLevs))
    } else {
      color <- colScale
    }
  } else {
    warning("colScale needs to be logical or character of colors.  This defaults to black")
    color <- rep("black", length = NtrxLevs)
  }
  # set lty pattern
  ltys <- c("solid", "73", "44", "6333", "27", "1343", "1234", "2262", "2434", "4153")[1:NtrxLevs]
  # plot first set of points, other points will be added later if !Ropen


  # Make sure y limits are wide enough
  minY <- floor(min(plotPts))
  maxY <- ceiling(max(plotPts))
  if (is.null(yLim)) {
    yLim <- c(minY, maxY)
  } else {
    if (yLim[1] > minY | yLim[2] < maxY) {
      warning("yLim doesn't cover all points and has been changed")
    }
    yLim[1] <- ifelse(yLim[1] > minY, minY, yLim[1])
    yLim[2] <- ifelse(yLim[2] < maxY, maxY, yLim[2])
  }
  plot(x = 1:NfactLevs, y = if (Ropen) {
    plotPts
  } else {
    plotPts[grep(paste("^", trxLevs[1], sep = ""), names(plotPts))]
  }, lty = ltys[1], ylim = yLim, main = pTitle, type = "b", xlab = xLab, ylab = yLab, las = 1, pch = rep(
    pchPts[1],
    NtrxLevs
  ), col = rep(color[1], NtrxLevs), xaxt = "n")

  # add factor levels to x-axis
  axis(1, at = 1:NfactLevs, labels = factLevs)

  # add dotted horizontal lines
  ax <- axTicks(2)
  if (refLines) {
    for (i in 1:length(ax)) {
      lines(x = c(0, (NfactLevs + 1)), y = c(ax[i], ax[i]), lty = c("19"))
    }
  }
  # add other points
  if (!Ropen) {
    for (i in 2:NtrxLevs) {
      points(
        x = 1:NfactLevs, y = plotPts[grep(paste("^", trxLevs[i], sep = ""), names(plotPts))],
        lty = ltys[i], pch = rep(pchPts[i], NtrxLevs), col = rep(color[i], NtrxLevs), type = "b"
      )
    }
    # add legend
    if (Legend) {
      legend(LegendLoc, col = color, pch = pchPts, lty = ltys, legend = trxLevs, bg = "white", cex = LegendCex)
    }
  }

  # set up matrix for uwLatex() redo mean and medVals to get order right for table
  N <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], length)
  meanVals <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], mean)
  medVals <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], median)
  sdVals <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], sd)
  minVals <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], min)
  maxVals <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], max)
  Vals25th <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], quantile, prob = 0.25)
  Vals75th <- tapply(dat[[metricName]], dat[[factorName]]:dat[[trxName]], quantile, prob = 0.75)

  mat1 <- round(cbind(
    N = N, Mean = meanVals, SD = sdVals, Min. = minVals, Q1 = Vals25th, Median = medVals,
    Q3 = Vals75th, Max. = maxVals
  ), numDec)
  rownames(mat1) <- NULL
  if (Ropen) {
    mat1 <- cbind(`NULL` = factLevs, mat1)
  } else {
    fact <- rep("", NfactLevs * NtrxLevs)
    fact[1:length(fact) %% NtrxLevs == 1] <- levels(dat[[factorName]])
    mat1 <- cbind(`NULL` = fact, TRT = trxLevs, mat1)
  }
  if (!is.null(firstCol.name)) {
    colnames(mat1)[1] <- firstCol.name
  }

  if (pairDiffTab) {
    # Get matrix of paired difference dat
    diff <- diffFunction(dat,
      variable = metricName, TrxName = trxName, VisitName = factorName, ptID = ptIDname,
      Baseline = BaseLevel
    )
    # Remove Baseline values which are all 0's
    diff <- subset(diff, diff$timeCol != BaseLevel)
    diff$timeCol <- drop.levels(diff$timeCol, reorder = FALSE)

    Ndiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, length)
    meandiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, mean)
    meddiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, median)
    sddiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, sd)
    mindiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, min)
    maxdiff <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, max)
    diff25th <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, quantile, prob = 0.25)
    diff75th <- tapply(diff$diffCol, diff$timeCol:diff$diffTRT, quantile, prob = 0.75)

    matDiff <- round(cbind(
      N = Ndiff, Mean = meandiff, SD = sddiff, Min. = mindiff, Q1 = diff25th,
      Median = meddiff, Q3 = diff75th, Max. = maxdiff
    ), numDec)
    rownames(matDiff) <- NULL
    if (Ropen) {
      matDiff <- cbind(`NULL` = levels(diff$timeCol), matDiff)
    } else {
      fact <- rep("", nlevels(diff$diffTRT) * nlevels(diff$timeCol))
      fact[1:length(fact) %% nlevels(diff$diffTRT) == 1] <- levels(diff$timeCol)
      matDiff <- cbind(`NULL` = fact, TRT = trxLevs, matDiff)
    }
    if (!is.null(firstCol.name)) {
      colnames(matDiff)[1] <- firstCol.name
    }
  }

  if (printPVals) {
    if (substr(pTest, 1, 1) == "a" | substr(pTest, 1, 1) == "k") {
      pairwise <- FALSE
      pTest <- ifelse(substr(pTest, 1, 1) == "a", "t", "w")
    } else {
      pairwise <- TRUE
    }
    if (pairDiffTab) {
      pvals <- uwPVals(
        data = diff, trxName = "diffTRT", trxControl = trxControl, metricName = "diffCol",
        factNames = "timeCol", pTest = pTest, pExclude = pExclude, pairwise = pairwise
      )
      if (!pairwise) {
        pCol <- rep("", nrow(matDiff))
        cCol <- pCol
        pCol[1:nrow(matDiff) %% nlevels(diff$diffTRT) == 0] <- pvals$pv
        cCol[1:nrow(matDiff) %% nlevels(diff$diffTRT) == 0] <- pvals$contrast
      } else {
        pCol <- pvals$pv
        cCol <- pvals$contrast
      }
      matDiff <- cbind(matDiff, Contrast = cCol, `P-Value` = pCol)
    } else {
      pvals <- uwPVals(
        data = dat, trxName = trxName, trxControl = trxControl, pTest = pTest, metricName = metricName,
        factNames = factorName, pExclude = pExclude, pairwise = pairwise
      )

      if (!pairwise | nlevels(dat[[trxName]]) == 2) {
        mtext(pvals$pv, side = 3, line = 0, cex = 0.7, at = 1:nlevels(dat[[factorName]]))
        pCol <- rep("", nrow(mat1))
        cCol <- pCol
        pCol[1:nrow(mat1) %% NtrxLevs == 0] <- pvals$pv
        cCol[1:nrow(mat1) %% NtrxLevs == 0] <- pvals$contrast
      } else {
        pCol <- pvals$pv
        cCol <- pvals$contrast
      }
      mat1 <- cbind(mat1, Contrast = cCol, `P-Value` = pCol)
    }
  }
  if (is.null(LatexFileName)) {
    warning("LatexFileName is NULL. Table is not saved")
    print(mat1)
  } else {
    tab1 <- uwLatex(
      mat = mat1, file = LatexFileName[1], cgroup = NULL, n.cgroup = NULL, rowLines = unlist(rowLines[1]),
      caption = unlist(caption[1]), ...
    )
  }
  if (pairDiffTab) {
    if (is.null(LatexFileName)) {
      warning("LatexFileName is NULL. Table is not saved")
      print(matDiff)
    } else {
      if (length(LatexFileName) != 2) {
        stop("LatexFileName needs to be length 2 with pairDiffTab==TRUE")
      }
      tab2 <- uwLatex(
        mat = matDiff, file = LatexFileName[2], cgroup = NULL, n.cgroup = NULL, caption = ifelse(is.null(unlist(caption[2])),
          paste("Paired Difference Table for", trxName, "by", factorName), unlist(caption[2])
        ), rowLines = unlist(rowLines[2]),
        ...
      )
    }
  }
}
