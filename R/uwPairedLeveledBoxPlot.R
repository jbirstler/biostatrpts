#' Box Plots of Paired Numeric Variables over different Levels
#'
#' This function is meant for the creation of box plots for paired numeric
#' variables over the levels of a factor.  This function also produces the
#' LaTeX code needed to create the two corresponding LaTeX tables summarizing
#' the box plots numerically.
#'
#' Additional Packages Required: uwLeveledBoxPlot() and uwLatex() are embedded
#' in uwPairedLevelBoxPlot
#'
#' Function matches ptID readings for pre (first level of pairName) to
#' corresponding post (second level of pairName) reading.  A Wilcoxon Signed
#' Rank Test for paired data is conducted on the matched pairs and that p-value
#' is reported, or Paired T-test if pTest='t'.  The difference box is shown as
#' post-pre.
#'
#' @param pairData Data frame with relevant variables.
#' @param pairName String of the factor variable name in pairData that gives
#' the two levels being compared.
#' @param metricName String of the variable name that represents the numeric
#' variable in pairData
#' @param factorName String of the variable name in pairData that gives the
#' levels that the comparisons will be made at separately
#' @param ptID String of the variable name that represents the patient
#' identification variable in pairData
#' @param LatexFileName String of length 2. Giving the folder and files (ending
#' in .tex) for the LaTeX tables to be saved.
#' @param pTitle String of the Title of the plot of the individual box plots.
#' @param yLab String of the y-axis label of the plot of the individual box
#' plots.
#' @param pTest String. 'w' gives Wilcoxon Signed Rank p-values on the vector
#' of paired differences for each level of factorName. 't' gives Student's
#' T-test p-values.  Defaults to 'w'
#' @param plotMean Logical. TRUE -> Gives a line in the box plot that shows
#' where the mean is.
#' @param pOutliers Logical.  TRUE -> Plots outliers as circles
#' @param lWhisker,hWhisker Percentages to depict the whiskers of the box plot.
#' l for lower whisker and h for higher whisker.
#' @param lHinge,hHinge Percentages where the corners of the boxes be. Default
#' is 1st and 3rd quartiles
#' @param boxWex Numeric between 0 and 1 for width of boxes.
#' @param printPVals Logical. Do you want P-values to be reported?
#' @param yLim Limits of the y-axis. In format: c(start, finish)
#' @param showTab Logical. TRUE prints LaTeX table using structure() function
#' @param printDiff Logical. TRUE will return the difference vector
#' @param ... Any argument that can be passed to uwLeveledBoxPlot that isn't
#' already an argument in uwPairedLeveledBoxPlot.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S. and Frontier Science and
#' Technology Research Foundation, Patrick Lenon and Zekai Otles
#' @seealso uwLeveledBoxPlot, uwLatex
#' @export
#' @examples
#'
#'
#' trt <- rep(c("Case", "Control"), 60)
#' id <- rep(rep(1:20, rep(2, 20)), 3)
#' fact <- rep(c("A", "B", "C"), c(40, 40, 40))
#' met <- c(rnorm(40, 10), rnorm(40, 11), rnorm(40, 12))
#'
#' df <- data.frame(trt, id, fact, met)
#'
#' uwPairedLeveledBoxPlot(
#'   pairData = df, pairName = "trt", metricName = "met",
#'   factorName = "fact", ptID = "id",
#'   LatexFileName = NULL,
#'   yLab = "Normal", pTest = "w",
#'   pTitle = "Example of uwPairedLeveledBoxPlot",
#'   plotMean = TRUE, pOutliers = FALSE,
#'   lWhisker = .05, hWhisker = .95,
#'   rowLines = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
#'   lHinge = .25, hHinge = .75,
#'   boxWex = 0.75, printPVals = TRUE
#' )
uwPairedLeveledBoxPlot <- function(pairData, pairName, metricName, factorName, ptID, LatexFileName, pTitle = NULL,
                                   yLab = NULL, pTest = c("w", "t"), plotMean = FALSE, pOutliers = FALSE, lWhisker = 0.05, hWhisker = 0.95,
                                   lHinge = 0.25, hHinge = 0.75, boxWex = 0.75, printPVals = TRUE, yLim = NULL, showTab = FALSE, printDiff = FALSE,
                                   ...) {
  pairData[[pairName]] <- as.factor(pairData[[pairName]])
  pairData[[ptID]] <- as.factor(pairData[[ptID]])
  if (length(LatexFileName) != 2) {
    warning("LatexFileName needs to be of length 2")
  }
  if (nlevels(pairData[[pairName]]) != 2) {
    warning("pairName needs to have two levels only")
  }

  layout(matrix(c(1, 1, 2, 2), nrow = 2, byrow = TRUE)) # Makes graphs on top of each other

  uwLeveledBoxPlot(
    allData = pairData, trxName = pairName, metricName = metricName, lowfactName = factorName,
    LatexFileName = LatexFileName[1], pTitle = pTitle, yLab = yLab, printPVals = FALSE, yLim = yLim,
    Legend = FALSE, ...
  )


  # Remove records that have an 'NA' in the metric column
  pairData <- pairData[!is.na(pairData[[metricName]]), ]

  # Get Difference by each level of factorName
  lFact <- levels(pairData[[factorName]])
  nFact <- length(lFact)
  diffVec <- c()
  diffFact <- c()
  idsVec <- c()
  for (i in 1:nFact) {
    thisFact <- subset(pairData, subset = pairData[[factorName]] == lFact[i])
    # Check if more than 1 reading per ptID in ith level of factorName
    if (max(table(thisFact[[pairName]], thisFact[[ptID]])) > 1) {
      stop("There is a ptID with more than one reading in a level of pairName in level of factorName")
    }

    # Find eligible ptID's that have both pre and post
    IDlevels <- levels(thisFact[[ptID]])
    pairVector <- levels(thisFact[[pairName]])

    preIDs <- levels(factor(thisFact[[ptID]][thisFact[[pairName]] == pairVector[1]]))
    postIDs <- levels(factor(thisFact[[ptID]][thisFact[[pairName]] == pairVector[2]]))

    checkPre <- IDlevels %in% preIDs
    checkPost <- IDlevels %in% postIDs
    eligibleIDs <- IDlevels[checkPre & checkPost]

    # Narrow data based on eligible ptID's
    pairedData <- subset(thisFact, subset = (thisFact[[ptID]] %in% eligibleIDs))
    pairedData[[ptID]] <- factor(pairedData[[ptID]])

    # Make Sure ordering is correct

    pairedData <- pairedData[order(pairedData[[ptID]], pairedData[[pairName]]), ]
    ids <- levels(pairedData[[ptID]])
    idsVec <- c(idsVec, ids)

    # Find Differences
    diff <- tapply(pairedData[[metricName]], pairedData[[ptID]], diff)
    diffVec <- c(diffVec, diff)

    # Specify
    fact <- rep(lFact[i], length(diff))
    diffFact <- c(diffFact, fact)
  }

  diffData <- data.frame(diffVec, diffFact, idsVec)
  diffData$diffFact <- ordered(diffData$diffFact, lFact)

  # Create boxplots without plotting
  bp1 <- boxplot(diffData$diffVec ~ diffData$diffFact, axes = FALSE, plot = FALSE)

  Ns <- tapply(diffData$diffVec, diffData$diffFact, length)

  # alter bp Zekai-style to use the selected quantile intervals

  quantile05.values <- tapply(diffData$diffVec, diffData$diffFact, quantile, probs = lWhisker, na.rm = TRUE)

  bp1$stats[1, ] <- c(quantile05.values[is.na(quantile05.values) == FALSE])

  quantile95.values <- tapply(diffData$diffVec, diffData$diffFact, quantile, probs = hWhisker, na.rm = TRUE)

  bp1$stats[5, ] <- c(quantile95.values[is.na(quantile95.values) == FALSE])

  quantile25.values <- tapply(diffData$diffVec, diffData$diffFact, quantile, probs = lHinge, na.rm = TRUE)

  bp1$stats[2, ] <- c(quantile25.values[is.na(quantile25.values) == FALSE])

  quantile75.values <- tapply(diffData$diffVec, diffData$diffFact, quantile, probs = hHinge, na.rm = TRUE)

  bp1$stats[4, ] <- c(quantile75.values[is.na(quantile75.values) == FALSE])

  bp1$stats <- round(bp1$stats, 2)
  std.values <- round(tapply(diffData$diffVec, diffData$diffFact, sd, na.rm = TRUE), 2)
  median.values <- bp1$stats[3, ]
  mean.values <- round(tapply(diffData$diffVec, diffData$diffFact, mean, na.rm = TRUE), 3)

  min.values <- round(tapply(diffData$diffVec, diffData$diffFact, min, na.rm = TRUE), 2)
  max.values <- round(tapply(diffData$diffVec, diffData$diffFact, max, na.rm = TRUE), 2)

  # reset the matrix structure
  bp1$conf <- matrix(bp1$conf, nrow = 2)
  bp1$stats <- matrix(bp1$stats, nrow = 5)
  pVal <- c()
  pStr <- c()


  if (printPVals) {
    if (substr(pTest[1], 1, 1) != "w" & substr(pTest[1], 1, 1) != "t") {
      stop("pTest is not a valid choice, must be w or t")
    }
    for (i in 1:nFact) {
      if (substr(pTest[1], 1, 1) == "w") {
        pVal[i] <- round(wilcox.test(diffData$diffVec[diffData$diffFact == lFact[i]], exact = FALSE, )$p.value, 3)
      } else {
        pVal[i] <- round(t.test(diffData$diffVec[diffData$diffFact == lFact[i]])$p.value, 3)
      }
      if (pVal[i] == 0) {
        pVal[i] <- "< 0.001"
        pStr[i] <- paste("pDiff ", pVal[i], sep = "")
      } else {
        pStr[i] <- paste("pDiff = ", pVal[i], sep = "")
      }
    }
  }

  if (plotMean) {
    bp1$stats[3, ] <- mean.values
    bxp(bp1,
      xaxt = "n", boxwex = boxWex, ylab = paste("Diff. of", yLab, sep = " "), outline = pOutliers,
      axes = FALSE, main = "Paired Differences"
    )

    # Now restore the proper median values
    bp1$stats[3, ] <- median.values
  } else {
    bxp(bp1,
      xaxt = "n", boxwex = boxWex, outline = pOutliers, axes = FALSE, medlty = "blank", main = "Paired Differences",
      ylab = paste("Diff. of", yLab, sep = " ")
    )
  }

  # add median values in points form
  medLoc <- seq(1:length(bp1$stats[1, ]))
  points(x = medLoc, y = median.values, pch = c(16))

  # Label bottom axis with pair levels and sample size
  mtext(lFact, side = 1, line = 1, at = c(1:nFact), cex = 0.7)
  mtext(Ns, side = 1, line = 0, at = c(1:nFact), cex = 0.6)
  mtext("Pairs", side = 1, line = 0, at = par("usr")[1], adj = 1, cex = 0.6)

  # Place horizontal dotted line at 0

  abline(h = 0, lty = 2)

  # Label top with p-values
  if (printPVals) {
    mtext(pStr, side = 3, at = c(1:nFact), cex = 0.7)
  }

  # axes
  axis(2, at = axTicks(2), labels = paste(axTicks(2), sep = ""), las = 2, cex.axis = 0.8)

  box()

  table.values <- cbind(
    Factor = lFact, Pairs = Ns, Mean = format(mean.values, digits = 2, nsmall = 2),
    SD = format(std.values, digits = 2, nsmall = 2), Min = format(min.values, digits = 2, nsmall = 2),
    P5 = format(bp1$stats[1, ], digits = 2, nsmall = 2), Q1 = format(bp1$stats[2, ], digits = 2, nsmall = 2),
    Median = format(bp1$stats[3, ], digits = 2, nsmall = 2), Q3 = format(bp1$stats[4, ],
      digits = 2,
      nsmall = 2
    ), P95 = format(bp1$stats[5, ], digits = 2, nsmall = 2), Max = format(max.values,
      digits = 2, nsmall = 2
    )
  )
  colnames(table.values)[1] <- ""

  if (printPVals) {
    table.values <- cbind(table.values, Contrast = rep("Diff=0", nFact), `P-Value` = pVal)
  }

  rownames(table.values) <- NULL

  if (printPVals) {
    col.just <- c("|l|", rep("c", 10), "|r", "r|")
  } else {
    col.just <- c("|l|", rep("c", 9), "c|")
  }
  ltxTbl <- uwLatex(
    mat = table.values, file = LatexFileName[2], caption = paste("Diff. of", yLab, sep = " "),
    col.just = col.just, rowLines = FALSE, size = "small"
  )

  if (showTab) {
    structure(list(file = LatexFileName[2]), class = "latex")
  }

  if (printDiff) {
    return(diffData)
  }
}
