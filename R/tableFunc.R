#' Format for printing frequency and percentages
#'
#' Meant to be used in creating R matrix to be exported as a table
#'
#' table(fvar,gvar) and 100*prop.table(table(fvar,gvar),2) are used to create N
#' (\%) for each level of fvar by gvar.  If only 1 level is needed (i.e. Yes/No
#' variable only needs Yes to be reported) then level can specify which level
#' to print
#'
#' @param data Data frame where nvar and gvar can be found
#' @param fvar Character string of column in data of factor data
#' @param gvar Character string of column in data of grouping data
#' @param level Character string of level of fvar to be reported.  If null then
#' all levels are printed
#' @param digits Number of decimals to be printed
#' @return Returns a vector of strings of N (\%) from table(fvar,gvar) with
#' columns printed first.  If gvar is NULL then N (\%) of fvar is returned.
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso chisqFunc
#' @export
#' @examples
#'
#' set.seed(784)
#' outcome <- factor(sample(c("No", "Yes"), replace = TRUE, size = 20))
#' group <- factor(rep(c("A", "B"), each = 10))
#' d.frame <- data.frame(outcome, group)
#'
#' tableFunc(d.frame, "outcome", "group", level = "Yes")
tableFunc <- function(data, fvar, gvar = NULL, level = NULL, digits = 1) {
  if (is.null(gvar)) {
    if (is.null(level)) {
      return(paste(table(data[[fvar]]), " (", rounds(100 * prop.table(table(data[[fvar]])), digits),
        "%)",
        sep = ""
      ))
    } else {
      return(paste(table(data[[fvar]])[level], " (", rounds(
        100 * prop.table(table(data[[fvar]]))[level],
        digits
      ), "%)", sep = ""))
    }
  } else {
    if (is.null(level)) {
      return(paste(table(data[[fvar]], data[[gvar]]), " (", rounds(100 * prop.table(table(
        data[[fvar]],
        data[[gvar]]
      ), 2), digits), "%)", sep = ""))
    } else {
      return(paste(table(data[[fvar]], data[[gvar]])[level, ], " (", rounds(100 * prop.table(table(
        data[[fvar]],
        data[[gvar]]
      ), 2)[level, ], digits), "%)", sep = ""))
    }
  }
}
