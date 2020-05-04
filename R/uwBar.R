#' Bar Plot function embedded in other bar plot functions
#'
#' Function called by other bar plot functions to actually create the bar plot.
#' This allows for a consistent look throughout multiple different functions
#'
#'
#' Additional Functions Required: rounds()
#'
#' This function is an adaptation of the traditional barplot() in which axes,
#' bars, bar values, and legends have the ability to be formatted in multiple
#' different ways.
#'
#' uwBar is called by any biostatrpts function that creates bar plots.
#'
#' @param table table or matrix used for plotting bar graph
#' @param pBeside (barplot)(logical) Stacked or side-by-side bars
#' @param pHoriz (barplot)(logical) Horizontal or vertical bars
#' @param lined (logical) Lined or solid colored bars
#' @param axLim (vector) Min and max axis values; x-axis for pHoriz=TRUE,
#' y-axis for pHoriz=FALSE
#' @param yLab (barplot)(string) Y-axis label
#' @param xLab (barplot)(string) X-axis label
#' @param printBarVals (logical) If TRUE the value of each bar is printed above
#' each bar
#' @param Legend (logical) If TRUE a legend is printed
#' @param LegendLoc (legend) Coordinates of string location of printed legend
#' if Legend=TRUE.  See ?legend
#' @param LegendCex (legend)(numeric) Sizing ratio of printed legend if
#' Legend=TRUE
#' @param mgp.y (par)(vector) Y-axis margin lines for title, label, and line
#' @param mgp.x (par)(vector) X-axis margin lines for title, label, and line
#' @param cex.names (barplot)(numeric) Sizing ratio of bar names
#' @param barNamesLas (0,1,2,or 3). Changes the verticality of the treatment
#' names for bar names and numeric axis. 1 and 2 are best for reporting
#' @param barNamesSeq (interger) Number indicating which bar labels to print.
#' This number is given as the 'by' attribute to seq().  So every barNameSeq
#' would be printed.  Default is 1, which will print all bar labels.  Helpful
#' when bar labels overlap when printing all of them.
#' @param barNamesAngle (integer) Number between 0 and 360 indicating how the
#' margin names should be rotated.  Useful when margin names are long strings
#' so you can angle them to all be seen
#' @param digits (rounds)(numeric) Number of decimal places to round to
#' @return Returns a list of (1) a matrix of the locations of each bar, (2) the
#' data used to create the bar plot, and (3) the axis values
#'
#' These values are used in the other biostatrpt functions for other items that
#' are printed like p-values.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @export
#' @examples
#'
#'
#' mat <- matrix(1:12, nrow = 3)
#' dimnames(mat) <- list(c("A", "B", "C"), c("D", "E", "F", "G"))
#'
#' uwBar(mat,
#'   pBeside = TRUE, pHoriz = FALSE, lined = FALSE,
#'   axLim = NULL, yLab = NULL, xLab = NULL, printBarVals = TRUE,
#'   Legend = TRUE, LegendLoc = "topright", LegendCex = 0.8,
#'   mgp.y = c(3, 1, 0), mgp.x = c(3, 1, 0), cex.names = 1, barNamesLas = 1,
#'   barNamesSeq = 1, barNamesAngle = 0, digits = 1
#' )
uwBar <- function(table, pBeside = TRUE, pHoriz = FALSE, lined = FALSE, axLim = NULL, yLab = NULL, xLab = NULL,
                  printBarVals = TRUE, Legend = FALSE, LegendLoc = "topright", LegendCex = 0.8, mgp.y = c(3, 1, 0), mgp.x = c(
                    3,
                    1, 0
                  ), cex.names = 1, barNamesLas = 1, barNamesSeq = 1, barNamesAngle = 0, digits = 1) {
  # Need to switch order of the table around if pHoriz=TRUE
  if (pHoriz) {
    if (length(dim(table)) == 1) {
      table <- rev(table)
    } else {
      tempcol <- colnames(table)
      temprow <- rownames(table)
      table <- matrix(rev(table), nrow = nrow(table), ncol = ncol(table))
      colnames(table) <- rev(tempcol)
      rownames(table) <- rev(temprow)
    }
  }
  if (!pBeside) {
    if (length(dim(table)) == 1) {
      table <- matrix(sum(table))
      dim(table) <- 1
    } else {
      table(apply(table, 2, rev))
    }
  }

  # If pBeside=FALSE then printBarVals has to be FALSE
  if (!pBeside & printBarVals == TRUE) {
    warning("If pBeside=FALSE then printBarVals has to be FALSE")
  }
  printBarVals <- ifelse(pBeside, printBarVals, FALSE)

  # Get Bar Colors based on number of rows in table
  if (length(dim(table)) == 1) {
    barCol <- "gray50"
  } else {
    barNum <- ceiling(seq(from = 10, to = 90, length = nrow(table)))
    barCol <- paste("gray", barNum, sep = "")
  }

  angles <- NULL
  dens <- NULL
  if (lined) {
    angles <- rep(c(0, 45, 0, 135), length = length(barCol))
    dens1 <- rev(ceiling(seq(from = 8, to = 40, length = floor(length(barCol) / 2))))
    dens <- rep(150, length(barCol))
    dens[seq(1, length(barCol), 1) %% 2 == 0] <- dens1
  }
  if (Legend) {
    legCol <- rev(barCol)
    legAng <- rev(angles)
    legDens <- rev(dens)
    if (pHoriz) {
      legText <- rev(rownames(table))
    } else {
      if (pBeside) {
        legText <- rownames(table)
      } else {
        legText <- rev(rownames(table))
      }
    }
  }
  if (!pHoriz & pBeside) {
    barCol <- rev(barCol)
    angles <- rev(angles)
    dens <- rev(dens)
  }
  if (is.null(axLim)) {
    if (pBeside) {
      axLim <- c(
        -0.02 * ceiling((max(table, na.rm = TRUE) + (max(table, na.rm = TRUE) * 0.1))),
        ceiling((max(table, na.rm = TRUE) + (max(table, na.rm = TRUE) * 0.1)))
      )
    } else {
      if (length(dim(table)) == 1) {
        axLim <- c(
          -0.02 * ceiling((max(table, na.rm = TRUE)) + (max(table, na.rm = TRUE)) * 0.1),
          ceiling((max(table, na.rm = TRUE)) + (max(table, na.rm = TRUE)) * 0.1)
        )
      } else {
        axLim <- c(-0.02 * ceiling((max(apply(table, 2, sum, na.rm = TRUE)) + (max(apply(table,
          2, sum,
          na.rm = TRUE
        )) * 0.1))), ceiling((max(apply(table, 2, sum, na.rm = TRUE)) + (max(apply(table,
          2, sum,
          na.rm = TRUE
        )) * 0.1))))
      }
    }
  } else if (axLim[1] == 0) {
    axLim <- c(-0.02 * ceiling(axLim[2] * 1.1), axLim[2])
  }
  if (pHoriz) {
    xlim <- axLim
    ylim <- NULL
  } else {
    xlim <- NULL
    ylim <- axLim
  }

  # Temp keep dimension names to delete them from bring printed
  dimNames <- dimnames(table)
  dimnames(table) <- NULL

  if (pBeside) {
    space <- c(0.2, 0.8)
  } else {
    space <- NULL
  }

  bp <- barplot(table,
    beside = pBeside, horiz = pHoriz, main = "", space = space, col = barCol, angle = angles,
    density = dens, xlab = "", ylab = "", xlim = xlim, ylim = ylim, axes = FALSE
  )
  box()
  # Restore dimension names to use them later
  dimnames(table) <- dimNames

  # Create dummy table for easy of labeling in graph
  labTab <- table

  if (!pBeside & length(dimnames(table)) == 2) {
    labTab <- apply(labTab, 2, sum)
  }

  # Add Legend
  if (Legend) {
    legend(LegendLoc, legend = legText, fill = legCol, cex = LegendCex, angle = legAng, density = legDens)
  }

  # Add Axes
  if (pHoriz) {
    axs <- axis(side = 1, tcl = 0, mgp = mgp.x, cex.axis = cex.names, las = barNamesLas)
    if (length(dimnames(labTab)) == 2) {
      if (barNamesAngle == 0) {
        axis(side = 2, tcl = 0, mgp = mgp.y, cex.axis = cex.names, las = barNamesLas, at = apply(
          bp,
          2, mean
        )[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)], labels = colnames(labTab)[seq(1,
          length(apply(bp, 2, mean)),
          by = barNamesSeq
        )])
      } else {
        text(
          x = par("usr")[1], y = apply(bp, 2, mean)[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)],
          labels = colnames(labTab)[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)], srt = barNamesAngle,
          xpd = TRUE, pos = 2, offset = 1, cex = cex.names
        )
      }
    } else {
      if (barNamesAngle == 0) {
        axis(side = 2, tcl = 0, mgp = mgp.y, cex.axis = cex.names, las = barNamesLas, at = bp[seq(1,
          length(bp),
          by = barNamesSeq
        )], labels = names(labTab)[seq(1, length(bp), by = barNamesSeq)])
      } else {
        text(x = par("usr")[1], y = bp[seq(1, length(bp), by = barNamesSeq)], labels = names(labTab)[seq(1,
          length(bp),
          by = barNamesSeq
        )], srt = barNamesAngle, xpd = TRUE, pos = 2, offset = 1, cex = cex.names)
      }
    }
  } else {
    axs <- axis(side = 2, tcl = -0.2, mgp = mgp.y, cex.axis = cex.names, las = barNamesLas)
    if (length(dimnames(labTab)) == 2) {
      if (barNamesAngle == 0) {
        axis(side = 1, tcl = 0, mgp = mgp.x, cex.axis = cex.names, las = barNamesLas, at = apply(
          bp,
          2, mean
        )[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)], labels = colnames(labTab)[seq(1,
          length(apply(bp, 2, mean)),
          by = barNamesSeq
        )])
      } else {
        text(
          y = par("usr")[3], x = apply(bp, 2, mean)[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)],
          labels = colnames(labTab)[seq(1, length(apply(bp, 2, mean)), by = barNamesSeq)], srt = barNamesAngle,
          xpd = TRUE, pos = 1, offset = 2.5, cex = cex.names
        )
      }
    } else {
      if (barNamesAngle == 0) {
        axis(side = 1, tcl = 0, mgp = mgp.x, cex.axis = cex.names, las = barNamesLas, at = bp[seq(1,
          length(bp),
          by = barNamesSeq
        )], labels = names(labTab)[seq(1, length(bp), by = barNamesSeq)])
      } else {
        text(
          y = par("usr")[3], x = bp[seq(1, length(bp), by = barNamesSeq)], labels = names(labTab)[seq(1,
            length(bp),
            by = barNamesSeq
          )], srt = barNamesAngle, xpd = TRUE, pos = 1, offset = 2.5,
          cex = cex.names
        )
      }
    }
  }
  # Add Axis Labels
  if (!is.null(yLab)) {
    title(ylab = yLab, line = mgp.y[1])
  }
  if (!is.null(xLab)) {
    title(xlab = xLab, line = mgp.x[1])
  }

  # Add Bar Values
  if (printBarVals) {
    if (pHoriz) {
      text(x = (table + (0.05 * max(axs))), y = bp, labels = rounds(table, digits = digits), cex = 0.6)
    } else {
      text(x = bp, y = (table + (0.05 * max(axs))), labels = rounds(table, digits = digits), cex = 0.6)
    }
  }

  return(list(bp, table, axs))
}
