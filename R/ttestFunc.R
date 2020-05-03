ttestFunc <- function(data,nvar,gvar)
  {
    if(nlevels(data[[gvar]])>2)
      return(ifelse(anova(lm(data[[nvar]]~data[[gvar]]))$Pr[1]<0.001,"< 0.001",
                    rounds(anova(lm(data[[nvar]]~data[[gvar]]))$Pr[1],3)))
    else
      return(ifelse(t.test(data[[nvar]]~data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(t.test(data[[nvar]]~data[[gvar]])$p.v,3)))
  }
