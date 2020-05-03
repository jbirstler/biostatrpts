#' Format for printing p-value from t-test
#' 
#' Meant to be used in creating R matrix to be exported as a table
#' 
#' If the p-value is less than 0.0005 then "< 0.001" is returned otherwise
#' p-value rounded to 3 decimals is returned
#' 
#' @param data Data frame where nvar and gvar can be found
#' @param nvar Character string of column in data of numeric data
#' @param gvar Character string of column in data of grouping data
#' @return Returns a string of the p-value from a t-test or anova if gvar has
#' more than 2 levels
#' @author University of Wisconsin-Madison Biostatistics and Medical
#' Informatics Department, Scott Hetzel M.S.
#' @seealso meanFunc
#' @examples
#' 
#' outcome <- rnorm(20)
#' group <- factor(rep(c("A","B"),each=10))
#' d.frame <- data.frame(outcome,group)
#' 
#' ttestFunc(d.frame,"outcome","group")
#' 
ttestFunc <- function(data,nvar,gvar)
  {
    if(nlevels(data[[gvar]])>2)
      return(ifelse(anova(lm(data[[nvar]]~data[[gvar]]))$Pr[1]<0.001,"< 0.001",
                    rounds(anova(lm(data[[nvar]]~data[[gvar]]))$Pr[1],3)))
    else
      return(ifelse(t.test(data[[nvar]]~data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(t.test(data[[nvar]]~data[[gvar]])$p.v,3)))
  }
