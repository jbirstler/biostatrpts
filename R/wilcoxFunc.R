wilcoxFunc <- function(data,nvar,gvar)
  {
    if(nlevels(data[[gvar]])>2)
      return(ifelse(kruskal.test(data[[nvar]]~data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(kruskal.test(data[[nvar]]~data[[gvar]])$p.v,3)))
    else
      return(ifelse(wilcox.test(data[[nvar]]~data[[gvar]])$p.v<0.001,"< 0.001",
                    rounds(wilcox.test(data[[nvar]]~data[[gvar]])$p.v,3)))
  }
