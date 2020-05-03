chisqFunc <- function(data,fvar,gvar,fisher=FALSE)
  {
    if(fisher)
      return(ifelse(fisher.test(data[[fvar]],data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(fisher.test(data[[fvar]],data[[gvar]])$p.v,3)))
    else
      return(ifelse(chisq.test(data[[fvar]],data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(chisq.test(data[[fvar]],data[[gvar]])$p.v,3)))
  }
