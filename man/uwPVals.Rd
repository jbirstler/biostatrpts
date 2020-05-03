\name{uwPVals}
\alias{uwPVals}
\title{Calculate P-Values for Various Tests}
\description{Function imbedded in many biostatrpts functions to
  calculate P-Values based on type of test desired and contrasts
  desired}

\usage{
uwPVals(data,
        factNames=NULL,
        metricName=NULL,
        trxName,
        trxControl=NULL,
        pTest=c("wilcox","t.test","fisher","chisq"),
        pairwise=TRUE,
        pAdjust=NULL,
        pExclude=NULL,
        pInclude=list(list(NULL,NULL)),
        abbrevN=1)
}
\arguments{
  \item{data}{Data frame containing the columns needed to run the
    function}
  \item{factNames}{(string) Can be a vector of length 1 or 2.  If length
    2, the first variable string listed is considered the upper level
    factor and the second is the lower level factor.  Subsetting is done
    first for upper level then lower level}
  \item{metricName}{(string) Length can only be 1.  This is the variable
    name of the metric variable to be considered if there is one.}
  \item{trxName}{(string) Variable name of the treatment factor variable}
  \item{trxControl}{(string) One of the levels of trxName to be
    considered the control which all other levels should be compared
    too.  Not used if pairwise is FALSE.  If pairwise is true and
    trxControl is null then all two-way comparisons are considered}
  \item{pTest}{(string) Indicate which test you would like to have run.
    First letter is all that is looked at}
  \item{pairwise}{(logical) TRUE indicates two-way comparisons are to be
    looked at.  If trxControl is defined then it is two-way comparisons
    with the control level.  If trxControl is null then it is all
    two-way comparisons.  FALSE indicates a global test is to be used.
    trxControl is not looked at if FALSE.  "wilcox" runs Kruskal-Wallis
    test, "t.test" runs an ANOVA test, "fisher" and "chisq" run a
    Chi-Square test over all levels}
  \item{pAdjust}{(string) Indicates what, if any, p-value adjustment
    type you would like to use for two-way multiple comparisons. Default
    (NULL) means no adjustment, "h", for "holm" method, or "b" for
    "bonferroni" adjustment.  Number of comparisons is always the number
    of two-way combinations of the treatment levels, even for when
    trxControl is defined, and for when pExclude is used. Can only be
    used if pairwise=TRUE}
  \item{pExclude}{(string) Levels to not be considered in two-way contrasts.
    Not used if pairwise=FALSE or pInclude is used.  Format is different whether trxControl
    is defined or not.  If trxControl is non-null, pExclude is a vector of
    non-control levels whose contrast with the control will be excluded
    from being looked at.  If trxControl is null, pExclude is a vector
    of levels of trxName that is looked at two positions at a time to
    determine which contrast should be excluded.  For example if levels
    of trxName are "A","B","C","D" and we do not specify a trxControl,
    but we want comparsions with "D" to not be looked at pExclude should
    look like this: pExclude=c("A","D","B","D","C","D")}
  \item{pInclude}{(lists in a list)  Not used if pairwise=FALSE.  Gives
    the user the ability to specify which contrasts they would like
    to get p-values for.  The ability to combine treatment levels for a
    contrast is possible.  pInclude is a list and each contrast is a
    list of length two inside pInclude. For example, say you have levels
    "A","B","C", and "D".  And you want contrasts: A.B, A.BCD, and AB.CD.  
    pInclude=list(list("A","B"),list("A",c("B","C","D")),list(c("A","B"),c("C","D")))
    All entries must be levels in trxName, and no levels can appear in
    both the left side and the right side of the contrast.}
  \item{abbrevN}{(positive numeric) The least amount of letters in the
    levels in trxName that still give unique distinction between levels}
}
\value{Returns a list of length 2. pvals and contrasts.  Specifying the
  p-values and the contrasts for each p-value}
\details{
  Sometimes situations arise in which a value of pTest is not a logical
  test to be run for the variables given.  Such as having two factor
  levels and no metric level, pTest="t.test" will not be allowed.

  Warning: Default is that there are no adjustments to the p-values for
  multiple testing.
}

\seealso{wilcox.test(), t.test(), fisher.test(), chisq.test(),
  kruskal.test(), anova()}
\examples{

TRT <- rep(c("A","B","C"), 120)
AE <- ordered(sample(c("None","Mild","Moderate","Severe"),360,replace=TRUE),
             c("None", "Mild", "Moderate", "Severe"))
Time <- rep(c("Baseline","Week 24"), each=180)
Weight <- rnorm(360,180,20)
data1 <- data.frame(TRT,AE,Weight,Time)

uwPVals(data1, factNames="AE", trxName="TRT", trxControl=NULL,
        pTest="f", pairwise=TRUE, pExclude=c("A","B"))

uwPVals(data1, factNames="AE", trxName="TRT", pairwise=FALSE, pTest="c")

uwPVals(data1, factNames="AE", metricName="Weight", trxName="TRT",
        trxControl="A", pTest="t", pairwise=TRUE)

uwPVals(data1, factNames=c("Time","AE"), metricName="Weight", trxName="TRT",
        trxControl=NULL, pTest="t", pairwise=TRUE)

# 1 factor, 1 metric vars
uwPVals(data1, factName="AE", metricName="Weight", trxName="TRT",
        pairwise=TRUE, pTest="t",trxControl=NULL,
pInclude=list(list("A","B"),list("A",c("B","C")), list(c("A","B"),"C")))

# 2 factors, 1 metric vars
uwPVals(data1, factName=c("Time","AE"), metricName="Weight", trxName="TRT",
        pairwise=TRUE, pTest="t",trxControl=NULL,
pInclude=list(list("A","B"),list("A",c("B","C")), list(c("A","B"),"C")))


}

