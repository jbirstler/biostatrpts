#' Box Plots of Numeric Variables
#'
#' This function is meant for the creation of box plots for numeric variables,
#' also allowing for comparisons of box plots through a categorical variable.
#' This function also produces the LaTeX code needed to create the
#' corresponding LaTeX table summarizing the box plots numerically.
#'
#' Uses uwPVals() to calculate p-values and uwLatex() to create the LaTeX
#' table)
#'
#' P-values do not have multiple comparison adjustments
#'
#' @param data Data frame with relevant variables.
#' @param trxName String of the variable name that represents the treatment in
#' data
#' @param metricName String of the variable name that represents the numeric
#' variable in data
#' @param LatexFileName (string) Giving the folder and file(ending in .tex) for
#' the LaTeX table to be saved. The default is NULL, which will create a table
#' in the current directory of the R session, and will have the file name of
#' the metricName.tex.
#' @param pTitle String of the Title of the plot.
#' @param yLab String of the y-axis label
#' @param xLab String of the x-axis label
#' @param plotMean Logical. TRUE -> Gives a line in the box plot that shows
#' where the mean is.
#' @param pOutliers Logical.  TRUE -> Plots outliers
#' @param boxWex Numeric between 0 and 1 for width of boxes.
#' @param printPVals Logical. Do you want P-values to be reported?
#' @param pTest uwPVals(), 'wilcox' or 't.test', which test do you want to use?
#' Wilcoxon Rank Sum Test or Student's T-test? First letter only is sufficient.
#' @param abbrevN uwPVals(), Whole number indicating how many letters should
#' abbreviation of the treatments levels should be.
#' @param pairwise uwPVals(), (logical) TRUE pairwise comparisons should be
#' made between treatment levels.  FALSE multi-level test occurs between all
#' levels of the treatment
#' @param pAdjust NULL for none, 'h' for 'holm', or 'b' for 'bonferroni'.
#' P-value adjustment methods.  See uwPVals for more information.
#' @param trxControl uwPVals(), only applies if pairwise=TRUE.  If NULL, all
#' two-way tests will be examined.  If a treatment level is indicated here as
#' the control level, then only two-way comparisons with the control and
#' non-controls will be made.
#' @param pExclude uwPVals(), only applies if pairwise=TRUE and pInclude is
#' NULL. If trxControl is NULL then this should be a vector of treatment
#' levels, looked at two at a time, that indicate that the p-value for that
#' contrast should not be reported.  Suppose the treatment levels are A,B,C,D
#' and you do not want the comparisons for B.C., B.D., and C.D..  pExclude
#' should look like this.  pExclude=c('B','C','B','D','C','D'). If trxControl
#' is defined then this should be a vector of non-control treatment levels that
#' will be looked at one at a time and that non-control's contrast will not be
#' reported
#' @param pInclude uwPVals(), only applies if pairwise=TRUE.  Gives the user
#' the ability to specify which contrasts they would like to get p-values for.
#' The ability to combine treatment levels for a contrast is possible.  See
#' help(uwPVals) for more details.
#' @param yLim Limits of the y-axis. In format: c(start, finish)
#' @param pStrWrap Logical. TRUE -> Creates two rows of p-value string on the
#' top margin instead of one.  Useful for many reported p-values.
#' @param pValCex Number between 0 and 1. Size of the p-value font in the top
#' margin.
#' @param titleCex Number between 0 and 1.  Size of Title text
#' @param LatexCaption String to be used as the caption for the LaTeX table.
#' Defaults to metricName.
#' @param firstCol.name (string) Name for first column, which is like a title
#' for the rows.  The default is TrxName if !Ropen, or catFactorName if Ropen.
#' @param digits Numeric between 0 to 20 indicating number of decimals to be
#' reported in the tables
#' @param cex.names cex for size of treatment labels on x-axis
#' @param ... Any other argument that can be passed to uwLatex
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S. and Frontier Science and
#' Technology Research Foundation, Patrick Lenon and Zekai Otles
#' @export
#' @examples
#'
#'
#' TRT <- c(rep("A", 20), rep("B", 20), rep("C", 20))
#' metric <- c(rnorm(20, 2, 1), rnorm(20, 3, 1), rnorm(20, 4, 1))
#'
#' dat <- data.frame(TRT, metric)
#'
#' uwBoxPlot(
#'   data = dat,
#'   trxName = "TRT",
#'   metricName = "metric",
#'   LatexFileName = NULL,
#'   showTab = FALSE,
#'   pTitle = "Example of uwBoxPlot",
#'   yLab = "Weight of Something (oz)",
#'   plotMean = FALSE, pOutliers = FALSE,
#'   boxWex = 0.75,
#'   Ropen = FALSE,
#'   printPVals = TRUE,
#'   pTest = "t",
#'   abbrevN = 1,
#'   pairwise = TRUE,
#'   trxControl = NULL,
#'   pExclude = NULL,
#'   pInclude = list(
#'     list(c("A", "B"), "C"), list("B", "C"),
#'     list("A", "C")
#'   ),
#'   yLim = NULL,
#'   pStrWrap = FALSE,
#'   pValCex = 0.7,
#'   LatexCaption = NULL,
#'   col.just = c("|l|", rep("c", 10), "|r", "r|"),
#'   firstCol.name = NULL
#' )
uwBoxPlot <- function(data, trxName = NULL, metricName, LatexFileName = NULL, pTitle = NULL, yLab = NULL,
                      xLab = NULL, plotMean = TRUE, pOutliers = TRUE, boxWex = 0.75, printPVals = TRUE, pTest = c(
                        "wilcox",
                        "t.test"
                      ), abbrevN = 1, pairwise = TRUE, pAdjust = NULL, trxControl = NULL, pExclude = NULL, pInclude = list(list(
                        NULL,
                        NULL
                      )), yLim = NULL, pStrWrap = FALSE, pValCex = 0.7, titleCex = 1, LatexCaption = NULL, firstCol.name = NULL,
                      digits = 1, cex.names = 0.7, ...) {
  if (!is.null(pInclude[[1]][[1]]) & !is.null(trxControl)) {
    trxControl <- NULL
    warning("trxControl is ignored when pInclude is defined, so trxControl set to NULL")
  }

  # Remove records that have an 'NA' in the metric column
  data <- data[!is.na(data[[metricName]]), ]

  # strip the input dataset down to only the relevant columns
  if (is.null(trxName)) {
    data <- subset(data, select = metricName)
    printPVals <- FALSE
  } else {
    data <- subset(data, select = c(trxName, metricName))
    data[[trxName]] <- factor(data[[trxName]]) # re-factor
  }

  uwbx <- uwBox(
    data = data, trxName = trxName, metricName = metricName, yLab = yLab, xLab = xLab, yLim = yLim,
    boxWex = boxWex, plotMean = plotMean, pOutliers = pOutliers
  )

  # PVALUES

  if (printPVals) {
    pvals <- uwPVals(
      data = data, metricName = metricName, trxName = trxName, pAdjust = pAdjust, trxControl = trxControl,
      pTest = pTest, pairwise = pairwise, pExclude = pExclude, pInclude = pInclude, abbrevN = abbrevN
    )
  }

  # add boxes around title, p-values, and sample sizes
  fb <- figureBox(
    boxPlot = TRUE, demoBarPlot = FALSE, pHoriz = FALSE, pTitle = pTitle, titleCex = titleCex,
    printPVals = printPVals, pStrWrap = pStrWrap
  )

  # print p-values at top of page

  if (printPVals) {
    # this will also include when pInclude is used
    if ((pairwise & is.null(trxControl))) {
      pv <- pvals$pv[pvals$pv != ""]
      contrast <- pvals$contrast[pvals$contrast != ""]
      pString <- paste(paste("p", contrast, sep = ""), "=", pv, sep = " ", collapse = "   ")
      if (pStrWrap) {
        stwrap <- strwrap(pString, 5)
        half <- floor(length(stwrap) / 2)
        pString1 <- paste(stwrap[1:half], collapse = "  ")
        pString2 <- paste(stwrap[(half + 1):length(stwrap)], collapse = "  ")
        mtext(pString1, side = 3, line = 1, cex = pValCex)
        mtext(pString2, side = 3, line = 0, cex = pValCex)
      } else {
        mtext(pString, side = 3, line = 0, cex = pValCex)
      }
    } else if (pairwise & !is.null(trxControl)) {
      mtext(pvals$pv, side = 3, line = 0, at = c(1:length(uwbx$n)), cex = pValCex)
    } else if (!pairwise) {
      mtext(paste(pvals$contrast, "=", pvals$pv), side = 3, cex = pValCex)
    }
  }
  # Label bottom axis with trx labels
  if (!is.null(trxName)) {
    if (length(grep("\n", levels(data[[trxName]]))) == 0) {
      mtext(levels(data[[trxName]]), side = 1, line = 1, at = c(1:length(uwbx$n)), cex = cex.names)
    } else {
      mtext(levels(data[[trxName]]), side = 1, line = 2, at = c(1:length(uwbx$n)), cex = cex.names)
    }
  }
  # Label bottom axis with sample sizes
  text(
    x = par("usr")[1] - 0.4 * fb[[2]], y = par("usr")[3] - 0.4 * fb[[1]], label = "N", xpd = TRUE,
    cex = 0.7
  )
  text(x = 1:length(uwbx$n), y = par("usr")[3] - 0.4 * fb[[1]], labels = uwbx$n, cex = 0.7, xpd = TRUE)

  axis(2,
    at = axTicks(2), tcl = -0.2, labels = paste(axTicks(2), sep = ""), las = 2, cex.axis = 0.8,
    mgp = c(3, 1, 0)
  )

  box()

  # put out basic stat table
  cgroup <- NULL
  n.cgroup <- NULL
  cgroup.just <- NULL

  if (is.null(trxName)) {
    LatexTab <- cbind(
      temp = metricName, N = uwbx$n, Mean = rounds(uwbx$mean, digits = digits), SD = rounds(uwbx$sd,
        digits = digits
      ), Min = rounds(uwbx$min, digits = digits), Q1 = rounds(uwbx$q25, digits = digits),
      Median = rounds(uwbx$median, digits = digits), Q3 = rounds(uwbx$q75, digits = digits), Max = rounds(uwbx$max,
        digits = digits
      )
    )
    colnames(LatexTab)[1] <- ""
  } else {
    LatexTab <- cbind(levels(data[[trxName]]),
      N = uwbx$n, Mean = rounds(uwbx$mean, digits = digits),
      SD = rounds(uwbx$sd, digits = digits), Min = rounds(uwbx$min, digits = digits), Q1 = rounds(uwbx$q25,
        digits = digits
      ), Median = rounds(uwbx$median, digits = digits), Q3 = rounds(uwbx$q75,
        digits = digits
      ), Max = rounds(uwbx$max, digits = digits)
    )

    if (printPVals) {
      rowLength <- max(nlevels(data[[trxName]]), length(pvals$contrast))
      if (nrow(LatexTab) != rowLength) {
        diff <- rowLength - nrow(LatexTab)
        for (i in 1:diff) {
          LatexTab <- rbind(LatexTab, rep("", times = ncol(LatexTab)))
        }
      }
      LatexTab <- cbind(LatexTab, Contrast = rep("", nrow(LatexTab)), `P-Value` = rep("", nrow(LatexTab)))
      LatexTab[1:length(pvals$contrast), 10] <- pvals$contrast
      LatexTab[1:length(pvals$contrast), 11] <- pvals$pv
    }

    if (is.null(firstCol.name)) {
      firstCol.name <- trxName
    }

    colnames(LatexTab)[1] <- firstCol.name
  }

  if (is.null(LatexCaption)) {
    LatexCaption <- pTitle
  }

  if (is.null(LatexFileName)) {
    print(LatexTab)
    warning("LatexFileName is NULL. Table not saved.")
  } else {
    w <- uwLatex(
      mat = LatexTab, file = LatexFileName, cgroup = cgroup, caption = LatexCaption, n.cgroup = n.cgroup,
      ...
    )
  }
}
