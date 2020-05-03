#' Calculates R-squared for lme model
#' 
#' Calculates R-squared for lme model
#' 
#' implements R2 from Edwards, Muller, Wolfinger, Qaqish and Schabenberger,
#' 2008.  need the aod package for wald.test function
#' 
#' @param obj lme object
#' @return R-square value
#' @examples
#' 
#' 
#' 
r2lme <-
  function(obj) {
    ## implements R2 from Edwards, Muller, Wolfinger, Qaqish and
    ## Schabenberger, 2008.
    require(aod) # need the aod package for wald.test function
    if (class(obj) != "lme") stop("lme object expected")
    n <- obj$dims[['N']] # the number of observations
    p <- ncol(coef(obj))# number of parameters
    df.error <- n - p
    wald.obj <- wald.test(b = fixef(obj), Sigma = vcov(obj), df = df.error, Terms = 2:p)
    wald.F <- as.numeric(wald.obj$result$Ftest[1])
    ( (p - 1) * 1 / df.error * wald.F ) / (1 + (p - 1 ) * 1 / df.error * wald.F)
  } 
