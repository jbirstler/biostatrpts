#' Format for printing p-value from Chi-square or Fisher exact test
#'
#' Meant to be used in creating R matrix to be exported as a table
#'
#' If the p-value is less than 0.0005 then '< 0.001' is returned otherwise
#' p-value rounded to 3 decimals is returned
#'
#' @param data Data frame where nvar and gvar can be found
#' @param fvar Character string of column in data of factor data
#' @param gvar Character string of column in data of grouping data
#' @param fisher Logical whether to use Fisher exact test or chi-square
#' @return Returns a string of the p-value from a Chi-square test or Fisher
#' exact test
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso tableFunc
#' @export
#' @examples
#'
#' set.seed(784)
#' outcome <- factor(sample(c("No", "Yes"), replace = TRUE, size = 20))
#' group <- factor(rep(c("A", "B"), each = 10))
#' d.frame <- data.frame(outcome, group)
#'
#' chisqFunc(d.frame, "outcome", "group", fisher = TRUE)
chisqFunc <- function(data, fvar, gvar, fisher = FALSE) {
  if (fisher) {
    return(ifelse(fisher.test(data[[fvar]], data[[gvar]])$p.v < 0.001, "< 0.001", rounds(fisher.test(
      data[[fvar]],
      data[[gvar]]
    )$p.v, 3)))
  } else {
    return(ifelse(chisq.test(data[[fvar]], data[[gvar]])$p.v < 0.001, "< 0.001", rounds(chisq.test(
      data[[fvar]],
      data[[gvar]]
    )$p.v, 3)))
  }
}
