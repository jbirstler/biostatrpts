#' Calculate P-Values for Various Tests
#'
#' Function imbedded in many biostatrpts functions to calculate P-Values based
#' on type of test desired and contrasts desired
#'
#' Sometimes situations arise in which a value of pTest is not a logical test
#' to be run for the variables given.  Such as having two factor levels and no
#' metric level, pTest='t.test' will not be allowed.
#'
#' Warning: Default is that there are no adjustments to the p-values for
#' multiple testing.
#'
#' @param data Data frame containing the columns needed to run the function
#' @param factNames (string) Can be a vector of length 1 or 2.  If length 2,
#' the first variable string listed is considered the upper level factor and
#' the second is the lower level factor.  Subsetting is done first for upper
#' level then lower level
#' @param metricName (string) Length can only be 1.  This is the variable name
#' of the metric variable to be considered if there is one.
#' @param trxName (string) Variable name of the treatment factor variable
#' @param trxControl (string) One of the levels of trxName to be considered the
#' control which all other levels should be compared too.  Not used if pairwise
#' is FALSE.  If pairwise is true and trxControl is null then all two-way
#' comparisons are considered
#' @param pTest (string) Indicate which test you would like to have run.  First
#' letter is all that is looked at
#' @param pairwise (logical) TRUE indicates two-way comparisons are to be
#' looked at.  If trxControl is defined then it is two-way comparisons with the
#' control level.  If trxControl is null then it is all two-way comparisons.
#' FALSE indicates a global test is to be used.  trxControl is not looked at if
#' FALSE.  'wilcox' runs Kruskal-Wallis test, 't.test' runs an ANOVA test,
#' 'fisher' and 'chisq' run a Chi-Square test over all levels
#' @param pAdjust (string) Indicates what, if any, p-value adjustment type you
#' would like to use for two-way multiple comparisons. Default (NULL) means no
#' adjustment, 'h', for 'holm' meth, or 'b' for 'bonferroni' adjustment.
#' Number of comparisons is always the number of two-way combinations of the
#' treatment levels, even for when trxControl is defined, and for when pExclude
#' is used. Can only be used if pairwise=TRUE
#' @param pExclude (string) Levels to not be considered in two-way contrasts.
#' Not used if pairwise=FALSE or pInclude is used.  Format is different whether
#' trxControl is defined or not.  If trxControl is non-null, pExclude is a
#' vector of non-control levels whose contrast with the control will be
#' excluded from being looked at.  If trxControl is null, pExclude is a vector
#' of levels of trxName that is looked at two positions at a time to determine
#' which contrast should be excluded.  For example if levels of trxName are
#' 'A','B','C','D' and we do not specify a trxControl, but we want comparsions
#' with 'D' to not be looked at pExclude should look like this:
#' pExclude=c('A','D','B','D','C','D')
#' @param pInclude (lists in a list) Not used if pairwise=FALSE.  Gives the
#' user the ability to specify which contrasts they would like to get p-values
#' for.  The ability to combine treatment levels for a contrast is possible.
#' pInclude is a list and each contrast is a list of length two inside
#' pInclude. For example, say you have levels 'A','B','C', and 'D'.  And you
#' want contrasts: A.B, A.BCD, and AB.CD.
#' pInclude=list(list('A','B'),list('A',c('B','C','D')),list(c('A','B'),c('C','D')))
#' All entries must be levels in trxName, and no levels can appear in both the
#' left side and the right side of the contrast.
#' @param abbrevN (positive numeric) The least amount of letters in the levels
#' in trxName that still give unique distinction between levels
#' @return Returns a list of length 2. pvals and contrasts.  Specifying the
#' p-values and the contrasts for each p-value
#' @seealso wilcox.test(), t.test(), fisher.test(), chisq.test(),
#' kruskal.test(), anova()
#' @export
#' @examples
#'
#'
#' TRT <- rep(c("A", "B", "C"), 120)
#' AE <- ordered(
#'   sample(c("None", "Mild", "Moderate", "Severe"), 360, replace = TRUE),
#'   c("None", "Mild", "Moderate", "Severe")
#' )
#' Time <- rep(c("Baseline", "Week 24"), each = 180)
#' Weight <- rnorm(360, 180, 20)
#' data1 <- data.frame(TRT, AE, Weight, Time)
#'
#' uwPVals(data1,
#'   factNames = "AE", trxName = "TRT", trxControl = NULL,
#'   pTest = "f", pairwise = TRUE, pExclude = c("A", "B")
#' )
#'
#' uwPVals(data1, factNames = "AE", trxName = "TRT", pairwise = FALSE, pTest = "c")
#'
#' uwPVals(data1,
#'   factNames = "AE", metricName = "Weight", trxName = "TRT",
#'   trxControl = "A", pTest = "t", pairwise = TRUE
#' )
#'
#' uwPVals(data1,
#'   factNames = c("Time", "AE"), metricName = "Weight", trxName = "TRT",
#'   trxControl = NULL, pTest = "t", pairwise = TRUE
#' )
#'
#' # 1 factor, 1 metric vars
#' uwPVals(data1,
#'   factName = "AE", metricName = "Weight", trxName = "TRT",
#'   pairwise = TRUE, pTest = "t", trxControl = NULL,
#'   pInclude = list(list("A", "B"), list("A", c("B", "C")), list(c("A", "B"), "C"))
#' )
#'
#' # 2 factors, 1 metric vars
#' uwPVals(data1,
#'   factName = c("Time", "AE"), metricName = "Weight", trxName = "TRT",
#'   pairwise = TRUE, pTest = "t", trxControl = NULL,
#'   pInclude = list(list("A", "B"), list("A", c("B", "C")), list(c("A", "B"), "C"))
#' )
uwPVals <- function(data, factNames = NULL, metricName = NULL, trxName, trxControl = NULL, pTest = c(
                      "wilcox",
                      "t.test", "fisher", "chisq"
                    ), pairwise = TRUE, pAdjust = NULL, pExclude = NULL, pInclude = list(list(
                      NULL,
                      NULL
                    )), abbrevN = 1) {

  # warnings for situations
  if (!is.null(trxControl)) {
    if (length(trxControl) != 1) {
      warning("trxControl can only be length of 1, only first entry is used")
      trxControl <- trxControl[1]
    }
  }
  pTest <- substr(pTest, 1, 1)
  if (length(pTest) != 1) {
    warning("pTest needs to be length one.  First entry is used")
    pTest <- pTest[1]
  }
  if (!(pTest %in% c("w", "t", "f", "c"))) {
    stop(warning("pTest not one of eligible tests"))
  }

  if (!is.list(pInclude) | !is.list(pInclude[[1]])) {
    stop("pInclude must be a list with lists for elements, see the default value for help")
  }
  if (is.null(pInclude[[1]][[1]])) {
    pInclude <- NULL
  }
  if (!is.null(pInclude)) {
    if (!pairwise) {
      warning("pInclude should by NULL if pairwise=FALSE. pInclude set to NULL")
      pInclude <- NULL
    }
    if (!is.null(pExclude)) {
      warning("pExclude should be NULL if pInclude is non-NULL, no exclusions took place")
      pExclude <- NULL
    }
    if (!is.null(pAdjust)) {
      warning("pAdjust should be NULL if pInclude is non-NULL, no p-value adjustment occurred")
      pAdjust <- NULL
    }
  }

  if (!is.null(pExclude)) {
    if (!pairwise) {
      warning("pExclude should be NULL when pairwise is FALSE, no exclusions took place")
      pExclude <- NULL
    } else {
      if (sum(pExclude %in% levels(data[[trxName]])) != length(pExclude)) {
        stop(warning("At least one of pExclude is not a level of trxName"))
      }
      if (is.null(trxControl) & (length(pExclude) %% 2) != 0) {
        stop(warning("pExclude must be even if pairwise=TRUE, and trxControl is NULL"))
      } else if (is.null(trxControl) & (length(pExclude) %% 2) == 0) {
        pExclude <- matrix(pExclude, nrow = 2, byrow = FALSE)
      }
      if (!is.null(trxControl) && (trxControl %in% pExclude)) {
        warning("trxControl should not be in pExclude. It has been removed from pExclude")
        pExclude <- pExclude[is.na(match(pExclude, trxControl))]
      }
    }
  }
  if (!is.null(pAdjust)) {
    if (!pairwise) {
      warning("pairwise must be TRUE for pAdjust to be used.  No p-value adjustment occurred")
      pAdjust <- NULL
    } else {
      if (substr(as.character(pAdjust), 1, 1) != "h" & substr(as.character(pAdjust), 1, 1) != "b") {
        warning("First character of pAdjust must be 'h' for 'holm' or 'b' for 'bonferroni'.  Defaulted to 'holm'")
        pAdjust <- "holm"
      } else if (substr(as.character(pAdjust), 1, 1) == "h") {
        pAdjust <- "holm"
      } else {
        pAdjust <- "bonferroni"
      }
    }
  }


  BarPVs <- function(dat, fName, tName, test, tCont, pair, pEx) {
    pv <- c()
    contrast <- c()

    abbrevTrx <- abbreviate(levels(dat[[tName]]), abbrevN)
    dat$mName <- as.numeric(dat[[fName]])
    if (pair) {
      if (is.null(tCont)) {
        trxComb <- combn(levels(dat[[tName]]), 2)
        abbrComb <- combn(abbrevTrx, 2)
        for (i in 1:ncol(trxComb)) {
          skip <- FALSE
          if (!is.null(pEx)) {
            for (j in 1:ncol(pEx)) {
              if (sum(trxComb[, i] %in% pEx[, j]) == 2) {
                skip <- TRUE
              }
            }
          }
          if (skip) {
            pv[i] <- ""
            contrast[i] <- ""
          } else {
            tempDat <- subset(dat, subset = dat[[tName]] == trxComb[1, i] | dat[[tName]] == trxComb[
              2,
              i
            ])
            tempDat[[tName]] <- ordered(tempDat[[tName]], c(trxComb[1, i], trxComb[2, i]))
            if (nlevels(drop.levels(tempDat[[tName]])) == 1) {
              pv[i] <- "NA"
              contrast[i] <- "NA"
            } else {
              tempTab <- table(tempDat[[fName]], tempDat[[tName]])
              # Makes sure there is data in this combination
              if (sum(tempTab == 0) == length(tempTab)) {
                pv <- ""
                contrast <- ""
              } else {
                if (test == "w") {
                  pv[i] <- round(wilcox.test(tempDat$mName[tempDat[[tName]] == trxComb[1, i]],
                    tempDat$mName[tempDat[[tName]] == trxComb[2, i]],
                    exact = FALSE
                  )$p.value, 3)
                }
                if (test == "f") {
                  pv[i] <- round(fisher.test(tempTab)$p.value, 3)
                }
                if (test == "c") {
                  pv[i] <- round(chisq.test(tempTab)$p.value, 3)
                }
                contrast[i] <- paste(abbrComb[1, i], ".", abbrComb[2, i])
              }
            }
          }
        }
      } else {
        for (i in 1:nlevels(dat[[tName]])) {
          if (levels(dat[[tName]])[i] != tCont & !(levels(dat[[tName]])[i] %in% pEx)) {
            tempDat <- subset(dat, dat[[tName]] == tCont | dat[[tName]] == levels(dat[[tName]])[i])
            tempDat[[tName]] <- ordered(tempDat[[tName]], c(tCont, levels(dat[[tName]])[i]))

            if (nlevels(drop.levels(tempDat[[tName]])) == 1) {
              pv[i] <- "NA"
              contrast[i] <- "NA"
            } else {
              tempTab <- table(tempDat[[fName]], tempDat[[tName]])
              # Makes sure there is data in this combination
              if (sum(tempTab == 0) == length(tempTab)) {
                pv <- ""
                contrast <- ""
              } else {
                if (test == "w") {
                  pv[i] <- round(
                    wilcox.test(tempDat$mName ~ tempDat[[tName]], exact = FALSE)$p.value,
                    3
                  )
                }
                if (test == "f") {
                  pv[i] <- round(fisher.test(tempTab)$p.value, 3)
                }
                if (test == "c") {
                  pv[i] <- round(chisq.test(tempTab)$p.value, 3)
                }
                contrast[i] <- paste(abbrevTrx[tCont == levels(dat[[tName]])], ".", abbrevTrx[i])
              }
            }
          } else {
            pv[i] <- ""
            contrast[i] <- ""
          }
        }
      }
    } else {
      tempTab <- table(dat[[fName]], dat[[tName]])
      # Makes sure there is data in this combination
      if (sum(tempTab == 0) == length(tempTab)) {
        pv <- ""
        contrast <- ""
      } else {
        if (test == "w") {
          pv <- round(kruskal.test(dat$mName ~ dat[[tName]])$p.value, 3)
          contrast <- "K-W"
        }
        if (test == "f") {
          pv <- round(fisher.test(tempTab)$p.value, 3)
          contrast <- "Fisher"
        }
        if (test == "c") {
          pv <- round(chisq.test(tempTab)$p.value, 3)
          contrast <- "Chi-Sq"
        }
      }
    }
    return(list(pv, contrast))
  }

  BoxPVs <- function(dat, mName, tName, test, tCont, pair, pEx) {
    pv <- c()
    contrast <- c()

    abbrevTrx <- abbreviate(levels(dat[[tName]]), abbrevN)

    if (pair) {
      if (is.null(tCont)) {
        trxComb <- combn(levels(dat[[tName]]), 2)
        abbrComb <- combn(abbrevTrx, 2)
        for (i in 1:ncol(trxComb)) {
          skip <- FALSE
          if (!is.null(pEx)) {
            for (j in 1:ncol(pEx)) {
              if (sum(trxComb[, i] %in% pEx[, j]) == 2) {
                skip <- TRUE
              }
            }
          }
          if (skip) {
            pv[i] <- ""
            contrast[i] <- ""
          } else {
            tempDat <- subset(dat, dat[[tName]] == trxComb[1, i] | dat[[tName]] == trxComb[2, i])
            tempDat[[tName]] <- ordered(tempDat[[tName]], c(trxComb[1, i], trxComb[2, i]))
            # Check to make sure there is at least 2 obs. per group to do statistcal test
            if (sum(summary(tempDat[[tName]]) <= 1) != 0) {
              pv[i] <- "NA"
              contrast[i] <- "NA"
            } else {
              if (test == "w") {
                pv[i] <- round(
                  wilcox.test(tempDat[[mName]] ~ tempDat[[tName]], exact = FALSE)$p.value,
                  3
                )
              }
              if (test == "t") {
                pv[i] <- round(t.test(tempDat[[mName]] ~ tempDat[[tName]])$p.value, 3)
              }
              contrast[i] <- paste(abbrComb[1, i], ".", abbrComb[2, i])
            }
          }
        }
      } else {
        for (i in 1:nlevels(dat[[tName]])) {
          if (levels(dat[[tName]])[i] != tCont & !(levels(dat[[tName]])[i] %in% pExclude)) {
            tempDat <- subset(dat, dat[[tName]] == tCont | dat[[tName]] == levels(dat[[tName]])[i])
            tempDat[[tName]] <- ordered(tempDat[[tName]], c(tCont, levels(dat[[tName]])[i]))
            # Check to make sure there is at least 2 obs. per group to do statistcal test
            if (sum(summary(tempDat[[tName]]) <= 1) != 0) {
              pv[i] <- "NA"
              contrast[i] <- "NA"
            } else {
              if (test == "w") {
                pv[i] <- round(
                  wilcox.test(tempDat[[mName]] ~ tempDat[[tName]], exact = FALSE)$p.value,
                  3
                )
              }
              if (test == "t") {
                pv[i] <- round(t.test(tempDat[[mName]] ~ tempDat[[tName]])$p.value, 3)
              }
              contrast[i] <- paste(abbrevTrx[tCont == levels(dat[[tName]])], ".", abbrevTrx[i])
            }
          } else {
            pv[i] <- ""
            contrast[i] <- ""
          }
        }
      }
    } else {
      if (test == "w") {
        pv <- round(kruskal.test(dat[[mName]] ~ dat[[tName]])$p.value, 3)
        contrast <- "K-W"
      }
      if (test == "t") {
        pv <- round(anova(lm(dat[[mName]] ~ dat[[tName]]))$Pr[1], 3)
        contrast <- "ANOVA"
      }
    }
    return(list(pv, contrast))
  }

  if (!is.null(factNames) & length(factNames) == 2) {
    UpFact <- factNames[1]
    LowFact <- factNames[2]
  } else if (!is.null(factNames) & length(factNames) == 1) {
    UpFact <- NULL
    LowFact <- factNames[1]
  } else {
    UpFact <- NULL
    LowFact <- NULL
  }

  # Override Process if pInclude is given and run uwPVals for each contrast level
  if (!is.null(pInclude)) {
    numContrasts <- length(pInclude)
    pvalues <- c()
    contras <- c()
    levels <- c()
    for (i in 1:numContrasts) {
      # Rename data so we don't overwrite it everytime uwPVals is run
      pData <- data
      # Isolate the contrast specified
      cont <- pInclude[[i]]
      # Check cont is a list of length 2
      if (length(cont) != 2) {
        stop("At least one of the list elements of pInclude is not of length 2")
      }
      # Isolate the left side of the contrast and make sure all levels are valid
      leftcont <- pInclude[[i]][[1]]
      if (sum(leftcont %in% levels(pData[[trxName]])) != length(leftcont)) {
        stop("At least one of the levels given in the left side of a
                      contrast of pInclude is not a level in trxName")
      }
      # Isolate the right side of the contrast and make sure all levels are valid
      rightcont <- pInclude[[i]][[2]]
      if (sum(rightcont %in% levels(pData[[trxName]])) != length(rightcont)) {
        stop("At least one of the levels given in the right side of a
                      contrast of pInclude is not a level in trxName")
      }
      # Make sure no level is in both leftcont and rightcont
      if (sum(leftcont %in% rightcont) != 0 | sum(rightcont %in% leftcont) != 0) {
        stop("At least one contrast has the same level in both left and right sides of contrast")
      }

      # Figure out which levels are not in the contrast
      contlevs <- c(leftcont, rightcont)
      notcontlevs <- levels(pData[[trxName]])[!(levels(pData[[trxName]]) %in% contlevs)]
      # Manipulate pData to refactor trxName by contrast and non-contrast levels
      leftfact <- paste(abbreviate(leftcont, abbrevN), collapse = "")

      rightfact <- paste(abbreviate(rightcont, abbrevN), collapse = "")
      labs <- c()
      length(labs) <- length(levels(pData[[trxName]]))

      labs[!(levels(pData[[trxName]]) %in% contlevs)] <- "FakeLev"
      labs[levels(pData[[trxName]]) %in% leftcont] <- leftfact
      labs[levels(pData[[trxName]]) %in% rightcont] <- rightfact
      trx.c <- as.character(pData[[trxName]])
      for (j in 1:nlevels(pData[[trxName]])) trx.c[pData[[trxName]] == levels(pData[[trxName]])[j]] <- labs[j]
      pData[[trxName]] <- factor(trx.c, levels = unique(labs))
      # pData[[trxName]] <- factor(pData[[trxName]], levels=levels(pData[[trxName]]), labels=labs)
      # pData[[trxName]] <- drop.levels(pData[[trxName]], reorder=FALSE) pData[[trxName]] <-
      # drop.levels(pData[[trxName]], reorder=FALSE)

      if ("FakeLev" %in% levels(pData[[trxName]])) {
        pExclude <- "FakeLev"
      } else {
        pExclude <- NULL
      }

      pvs <- uwPVals(
        data = pData, factNames = factNames, metricName = metricName, trxName = trxName,
        trxControl = leftfact, pTest = pTest, pairwise = TRUE, pExclude = pExclude, pInclude = list(list(
          NULL,
          NULL
        )), abbrevN = 21
      )

      pvalues <- c(pvalues, pvs$pv[pvs$pv != ""])
      contras <- c(contras, pvs$contrast[pvs$contrast != ""])
      levels <- c(levels, pvs$levels[pvs$contrast != ""])
      pvalues[pvalues == "NA"] <- ""
      contras[contras == "NA"] <- ""
    }
    # Manipulate to get the ordering correct
    pvalues <- c(matrix(pvalues, ncol = length(pvalues) / numContrasts, byrow = TRUE))
    contras <- c(matrix(contras, ncol = length(contras) / numContrasts, byrow = TRUE))
    levels <- c(matrix(levels, ncol = length(levels) / numContrasts, byrow = TRUE))

    PVlist <- list(pv = pvalues, contrast = contras, levels = levels)
    PVlist[[1]][PVlist[[1]] == 0] <- "< 0.001"

    return(PVlist)
  }


  # Indicates 2 factors and no metric var.
  if (!is.null(UpFact) & is.null(metricName)) {
    if (pTest == "t") {
      stop(warning("Cannot Run t.test with 2 factors and no metric variable"))
    }
    PVs <- c()
    Cons <- c()
    Levels <- c()
    for (k in 1:nlevels(data[[UpFact]])) {
      thisLev <- subset(data, data[[UpFact]] == levels(data[[UpFact]])[k])
      # thisLev[[trxName]] <- drop.levels(thisLev[[trxName]], reorder=FALSE)
      PV <- BarPVs(
        dat = thisLev, fName = LowFact, tName = trxName, test = pTest, tCont = trxControl,
        pair = pairwise, pEx = pExclude
      )
      if (!is.null(pAdjust)) {
        if (sum(is.nan(PV[[1]])) != length(PV[[1]])) {
          PV[[1]] <- p.adjust(as.numeric(PV[[1]]),
            meth
            = pAdjust, n = ncol(combn(
              nlevels(thisLev[[trxName]]),
              2
            ))
          )
        }
      }
      PVs <- c(PVs, PV[[1]])
      Cons <- c(Cons, PV[[2]])
      Levels <- c(Levels, rep(levels(data[[UpFact]])[k], length(PV[[2]])))
    }
    PVlist <- list(pv = PVs, contrast = Cons, levels = Levels)
  }

  # Indicates 1 factor and no metric var.
  if (is.null(UpFact) & !is.null(LowFact) & is.null(metricName)) {
    if (pTest == "t") {
      stop(warning("Cannot Run t.test with 1 factor and no metric variable"))
    }
    PV <- BarPVs(
      dat = data, fName = LowFact, tName = trxName, test = pTest, tCont = trxControl, pair = pairwise,
      pEx = pExclude
    )
    if (!is.null(pAdjust)) {
      if (sum(is.nan(PV[[1]])) != length(PV[[1]])) {
        PV[[1]] <- p.adjust(as.numeric(PV[[1]]),
          meth
          = pAdjust, n = ncol(combn(
            nlevels(data[[trxName]]),
            2
          ))
        )
      }
    }
    PVlist <- list(pv = PV[[1]], contrast = PV[[2]], levels = rep(LowFact, length(PV[[2]])))
  }

  # Indicates 2 factors and 1 metric var.
  if (!is.null(UpFact) & !is.null(metricName)) {
    if (pTest == "f" | pTest == "c") {
      stop(warning("Cannot run Fisher or Chi-Square test with a metric variable"))
    }
    PVs <- c()
    Cons <- c()
    Levs <- c()
    PVals <- c()
    Conts <- c()
    Levels <- c()
    for (k in 1:nlevels(data[[UpFact]])) {
      upLev <- subset(data, data[[UpFact]] == levels(data[[UpFact]])[k])
      upLev[[trxName]] <- drop.levels(upLev[[trxName]], reorder = FALSE)

      for (m in 1:nlevels(upLev[[LowFact]])) {
        # Check to make sure that level is in the data set
        if (sum(upLev[[LowFact]] == levels(upLev[[LowFact]])[m]) != 0) {
          thisLev <- subset(upLev, upLev[[LowFact]] == levels(upLev[[LowFact]])[m])

          PV <- BoxPVs(
            dat = thisLev, mName = metricName, tName = trxName, test = pTest, tCont = trxControl,
            pair = pairwise, pEx = pExclude
          )
          if (!is.null(pAdjust)) {
            if (sum(is.nan(PV[[1]])) != length(PV[[1]])) {
              PV[[1]] <- p.adjust(as.numeric(PV[[1]]),
                meth
                = pAdjust, n = ncol(combn(
                  nlevels(thisLev[[trxName]]),
                  2
                ))
              )
            }
          }
          PVs <- c(PVs, PV[[1]])
          Cons <- c(Cons, PV[[2]])
          Levs <- c(Levs, rep(paste(levels(upLev[[LowFact]])[m], ".", levels(data[[UpFact]])[k],
            sep = ""
          ), length(PV[[2]])))
        } else {
          if (abbrevN != 21) {
            if (!pairwise) {
              numContrasts <- 1
            } else if (is.null(trxControl)) {
              numContrasts <- ncol(combn(levels(data[[trxName]]), 2))
            } else {
              numContrasts <- nlevels(data[[trxName]])
            }
          } else {
            numContrasts <- 1
          }
          PVs <- c(PVs, rep("NA", numContrasts))
          Cons <- c(Cons, rep("NA", numContrasts))
          Levs <- c(Levs, rep(paste(levels(upLev[[LowFact]])[m], ".", levels(data[[UpFact]])[k],
            sep = ""
          ), numContrasts))
        }
      }
      PVals <- c(PVals, PVs)
      Conts <- c(Conts, Cons)
      Levels <- c(Levels, Levs)
      PVs <- c()
      Cons <- c()
      Levs <- c()
    }
    PVlist <- list(pv = PVals, contrast = Conts, levels = Levels)
  }

  # Indicates 1 factor and 1 metric var.
  if (is.null(UpFact) & !is.null(LowFact) & !is.null(metricName)) {
    if (pTest == "f" | pTest == "c") {
      stop(warning("Cannot run Fisher or Chi-Square test with a metric variable"))
    }
    PVs <- c()
    Cons <- c()
    Levels <- c()
    for (m in 1:nlevels(data[[LowFact]])) {
      thisLev <- subset(data, data[[LowFact]] == levels(data[[LowFact]])[m])
      # thisLev[[trxName]] <- drop.levels(thisLev[[trxName]], reorder=FALSE)
      PV <- BoxPVs(
        dat = thisLev, mName = metricName, tName = trxName, test = pTest, tCont = trxControl,
        pair = pairwise, pEx = pExclude
      )
      if (!is.null(pAdjust)) {
        if (sum(is.nan(PV[[1]])) != length(PV[[1]])) {
          PV[[1]] <- p.adjust(as.numeric(PV[[1]]),
            meth
            = pAdjust, n = ncol(combn(
              nlevels(thisLev[[trxName]]),
              2
            ))
          )
        }
      }
      PVs <- c(PVs, PV[[1]])
      Cons <- c(Cons, PV[[2]])
      Levels <- c(Levels, rep(levels(data[[LowFact]])[m], length(PV[[2]])))
    }
    PVlist <- list(pv = PVs, contrast = Cons, levels = Levels)
  }

  # Indicates 0 factors and 1 metric var.
  if (is.null(factNames) & !is.null(metricName)) {
    if (pTest == "f" | pTest == "c") {
      stop(warning("Cannot run Fisher or Chi-Square test with a metric variable"))
    }

    PV <- BoxPVs(
      dat = data, mName = metricName, tName = trxName, test = pTest, tCont = trxControl,
      pair = pairwise, pEx = pExclude
    )
    if (!is.null(pAdjust)) {
      if (sum(is.nan(PV[[1]])) != length(PV[[1]])) {
        PV[[1]] <- p.adjust(as.numeric(PV[[1]]),
          meth
          = pAdjust, n = ncol(combn(
            nlevels(data[[trxName]]),
            2
          ))
        )
      }
    }
    PVlist <- list(pv = PV[[1]], contrast = PV[[2]], levels = rep(NA, length(PV[[2]])))
  }

  PVlist[[1]][PVlist[[1]] == 0] <- "< 0.001"

  return(PVlist)
}
