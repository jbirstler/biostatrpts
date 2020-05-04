#' Format for printing mean (SD) by factor
#'
#' Meant to be used in creating R matrix to be exported as a table
#'
#' na.rm=TRUE is defaulted in the calculation of mean and SD
#'
#' @param data Data frame where nvar and gvar can be found
#' @param nvar Character string of column in data of which means will be
#' calculated
#' @param gvar Character string of column in data of which means will be
#' grouped by
#' @param digits Number of decimals the numbers will be rounded to
#' @return Returns a string of the format mean (SD) for each of the levels in
#' gvar. If gvar is null then mean (SD) of nvar is provided.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso medianFunc
#' @export
#' @import stats
#' @examples
#'
#' outcome <- rnorm(20)
#' group <- factor(rep(c("A", "B"), each = 10))
#' d.frame <- data.frame(outcome, group)
#'
#' meanFunc(d.frame, "outcome", "group")
meanFunc <- function(data, nvar, gvar = NULL, digits = 1) {
  if (is.null(gvar)) {
    return(paste(rounds(mean(data[[nvar]], na.rm = TRUE), digits), " (", rounds(
      sd(data[[nvar]], na.rm = TRUE),
      digits
    ), ")", sep = ""))
  } else {
    return(paste(rounds(tapply(data[[nvar]], data[[gvar]], mean, na.rm = TRUE), digits), " (", rounds(tapply(data[[nvar]],
      data[[gvar]], sd,
      na.rm = TRUE
    ), digits), ")", sep = ""))
  }
}
