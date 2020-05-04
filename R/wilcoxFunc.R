#' Format for printing p-value from wilcoxon rank sum test
#'
#' Meant to be used in creating R matrix to be exported as a table
#'
#' If the p-value is less than 0.0005 then '< 0.001' is returned otherwise
#' p-value rounded to 3 decimals is returned
#'
#' @param data Data frame where nvar and gvar can be found
#' @param nvar Character string of column in data of numeric data
#' @param gvar Character string of column in data of grouping data
#' @return Returns a string of the p-value from a wilcoxon rank sum test or
#' kruskal wallis test if gvar has more than 2 levels
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso medianFunc
#' @export
#' @examples
#'
#' outcome <- rnorm(20)
#' group <- factor(rep(c("A", "B"), each = 10))
#' d.frame <- data.frame(outcome, group)
#'
#' wilcoxFunc(d.frame, "outcome", "group")
wilcoxFunc <- function(data, nvar, gvar) {
  if (nlevels(data[[gvar]]) > 2) {
    return(ifelse(kruskal.test(data[[nvar]] ~ data[[gvar]])$p.v < 0.001, "< 0.001", rounds(kruskal.test(data[[nvar]] ~
    data[[gvar]])$p.v, 3)))
  } else {
    return(ifelse(wilcox.test(data[[nvar]] ~ data[[gvar]])$p.v < 0.001, "< 0.001", rounds(wilcox.test(data[[nvar]] ~
    data[[gvar]])$p.v, 3)))
  }
}
