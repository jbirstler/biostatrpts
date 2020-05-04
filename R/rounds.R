#' A function to round to a certain decimal but keep the zero as the last
#' decimal spot if necessary
#'
#' This function is used throughout multiple biostatrpts functions instead of
#' the traditional round() function.
#'
#' Embedded into multiple biostatrpts functions to round values neatly and have
#' consistent reporting.
#'
#' @param x A number or numeric vector whose digits will be rounded
#' @param digits Number of decimal places to round to
#' @param as.numeric If FALSE the number returned is a character string.  If
#' TRUE the number returned is numeric
#' @return A character or numeric string or vector
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso round()
#' @export
#' @examples
#'
#' #  See the difference in the print out of the two round functions for
#' #  rounding 0.0395 when rounding to 3 decimal places
#'
#' rounds(0.0395, 3)
#' round(0.0395, 3)
rounds <- function(x, digits = 0, as.numeric = FALSE) {
  ifelse(as.numeric, return(as.numeric(format(round(x, digits), nsmall = digits, trim = TRUE))), return(format(round(
    x,
    digits
  ), nsmall = digits, trim = TRUE)))
}
