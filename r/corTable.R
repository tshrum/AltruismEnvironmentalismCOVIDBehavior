library(xtable)

corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x, type = c("pearson"))$r 
  p <- rcorr(x, type = c("pearson"))$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

d %>%
  dplyr::select(NEP, sra, moralObl, 
           age , male , income , education , white , latino , trump , biden , lotterySwitchPoint ,  
           preexistCond , household65 , smallChildren , likelihoodFatalCovid , likelihoodCovid ,
           positiveCovid , knowCovid , incomeLoss , mentalHealthIndex ,
           maskFreqCommunity , socialGatherFreqCommunity , 
           popDensity , pastWeekCasesPC ,
           paidSickLeave , insured ,
           vaccinesCovid , antiVaxIndex , trustMedScientists , worriedCOVID) -> cortable

d %>%
  dplyr::select(NEP, sra, moralObl, 
                trump , lotterySwitchPoint ,  
                preexistCond , likelihoodCovid ,likelihoodFatalCovid , 
                knowCovid , incomeLoss , mentalHealthIndex ,
                vaccinesCovid , antiVaxIndex , trustMedScientists , worriedCOVID) -> cortable

corstarsl(cortable)
xtable(corstarsl(cortable))

