#' Format for printing median (IQR) or (Range) by factor
#'
#' Meant to be used in creating R matrix to be exported as a table
#'
#' na.rm=TRUE is defaulted in the calculation of median and IQR or min and max
#'
#' @param data Data frame where nvar and gvar can be found
#' @param nvar Character string of column in data of which medians will be
#' calculated
#' @param gvar Character string of column in data of which medians will be
#' grouped by
#' @param digits Number of decimals the numbers will be rounded to
#' @param IQR Logical: If TRUE then median (IQR) is printed.  If FALSE then
#' median (range) is printed
#' @return Returns a string of the format median (IQR or range) for each of the
#' levels in gvar.  If gvar is null then median (IQR or range) is returned for
#' nvar.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso meanFunc
#' @export
#' @import stats
#' @examples
#'
#' outcome <- rnorm(20)
#' group <- factor(rep(c("A", "B"), each = 10))
#' d.frame <- data.frame(outcome, group)
#'
#' medianFunc(d.frame, "outcome", "group", IQR = FALSE)
medianFunc <- function(data, nvar, gvar = NULL, digits = 1, IQR = TRUE) {
  if (is.null(gvar)) {
    if (IQR) {
      return(paste(rounds(median(data[[nvar]], na.rm = TRUE), digits), " (", rounds(quantile(data[[nvar]],
        probs = 0.25, na.rm = TRUE
      ), digits), " - ", rounds(quantile(data[[nvar]],
        probs = 0.75,
        na.rm = TRUE
      ), digits), ")", sep = ""))
    } else {
      return(paste(rounds(median(data[[nvar]], na.rm = TRUE), digits), " (", rounds(min(data[[nvar]],
        na.rm = TRUE
      ), digits), " - ", rounds(max(data[[nvar]], na.rm = TRUE), digits), ")", sep = ""))
    }
  } else {
    if (IQR) {
      return(paste(rounds(tapply(data[[nvar]], data[[gvar]], median, na.rm = TRUE), digits), " (",
        rounds(tapply(data[[nvar]], data[[gvar]], quantile, probs = 0.25, na.rm = TRUE), digits),
        " - ", rounds(
          tapply(data[[nvar]], data[[gvar]], quantile, probs = 0.75, na.rm = TRUE),
          digits
        ), ")",
        sep = ""
      ))
    } else {
      return(paste(rounds(tapply(data[[nvar]], data[[gvar]], median, na.rm = TRUE), digits), " (",
        rounds(tapply(data[[nvar]], data[[gvar]], min, na.rm = TRUE), digits), " - ", rounds(tapply(data[[nvar]],
          data[[gvar]], max,
          na.rm = TRUE
        ), digits), ")",
        sep = ""
      ))
    }
  }
}
