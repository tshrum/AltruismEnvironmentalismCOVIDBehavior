# Analysis for Pro-Environmental Attitudes Paper

load("data/prepped.R")

d %>% filter(invalidResponse == 0) -> d


source('r/dataCheck.R', echo=TRUE)


varsOrder <- c("NEP", "age", "male", "income", "education", "white", "latino", "trump", "biden",
               "riskAversion",
               "mentalHealthIndex",
               "likelihoodCovid", "preexistCond", "household65", "positiveCovid", 
               "knowCovid", "maskFreqCommunity", 
               "incomeLoss", 
               "paidSickLeave", "insured",
               "vaccinesCovid", "antiVaxIndex", "worriedCOVID")


#### Table 4: Looking at correlates of NEP and SRA ####
nep1 <- lm(NEP ~ age + male + income + scale(education) +  black + latino + trump + biden + scale(riskAversion) + division, data = d)
sra1 <- lm(sra ~ age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) + division, data = d)
mo1 <- lm(scale(moralObl) ~ age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) + division, data = d)

summary(nep1)
summary(sra1)
summary(mo1)
stargazer(nep1, sra1, mo1,
          no.space = T,
          column.sep.width = "1pt",
          suppress.errors = T,
          font.size = "small",
          report = "vc*",
          label = "NEP_SRA",
          dep.var.labels = c("NEP$^z$", "SRA$^z$", "Moral Obligation$^z$"),
          covariate.labels = c("Age (years)", "Male$^d$", "Income (1000s)", "Education$^z$", "White$^d$", "Latino$^d$",
                               "Trump-Voter$^d$", "Biden-Voter$^d$", "Risk Aversion$^z$", "East-South Central$^d$",
                               "Middle Atlantic$^d$", "Mountain$^d$", "New England$^d$", "Pacific$^d$",
                               "South Atlantic$^d$", "West-North Central$^d$", "West-South Central$^d$"))

linearHypothesis(nep1, "trump = biden")
linearHypothesis(sra1, "trump = biden")
linearHypothesis(mo1, "trump = biden")
confint(nep1, "trump", level = 0.95)
confint(sra1, "trump", level = 0.95)
confint(mo1, "trump", level = 0.95)
confint(nep1, "biden", level = 0.95)
confint(sra1, "biden", level = 0.95)
confint(mo1, "biden", level = 0.95)
confint(nep1, "male", level = 0.95)
confint(sra1, "male", level = 0.95)
confint(mo1, "male", level = 0.95)
confint(mo1, "latino", level = 0.95)
confint(nep1, "scale(riskAversion)", level = 0.95)
confint(sra1, "scale(riskAversion)", level = 0.95)
confint(nep1, "scale(education)", level = 0.95)
confint(sra1, "scale(education)", level = 0.95)
confint(mo1, "scale(education)", level = 0.95)
confint(sra1, "divisionNewEngland", level = 0.95)
confint(sra1, "divisionPacific", level = 0.95)
confint(nep1, "divisionEastSouthCentral", level = 0.95)
confint(mo1, "divisionMountain", level = 0.95)

#### Table 5: H1 VSE Index & NEP ####
# H1: People with pro-environmental attitudes have a higher willingness to adhere to COVID risk mitigation recommendations compared to those with less pro-environmental attitudes.

# Social Exposure Index ~ NEP #
nep1 <- lm(socialExposureIndex ~ NEP, data = d)
nep2 <- lm(socialExposureIndex ~ NEP + 
           age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +
           scale(mentalHealthIndex), 
         data = d)
nep3 <- lm(socialExposureIndex ~ NEP +
           age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
           scale(mentalHealthIndex) + 
           preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
           positiveCovid + knowCovid + scale(incomeLoss) + 
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(nep1)
summary(nep2)
summary(nep3)

confint(nep1, "NEP", level = 0.95)
confint(nep2, "NEP", level = 0.95)
confint(nep2, "scale(education)", level = 0.95)
confint(nep2, "scale(riskAversion)", level = 0.95)
confint(nep2, "scale(mentalHealthIndex)", level = 0.95)
confint(nep2, "trump", level = 0.95)

confint(nep3, "NEP", level = 0.95)
confint(nep3, "trump", level = 0.95)
confint(nep3, "pastWeekCasesPC", level = 0.95)
confint(nep3, "knowCovid", level = 0.95)
confint(nep3, "socialGatherFreqCommunity", level = 0.95)
confint(nep3, "preexistCond", level = 0.95)
confint(nep3, "household65", level = 0.95)
confint(nep3, "socialGatherFreqCommunity", level = 0.95)
confint(nep3, "smallChildren", level = 0.95)
confint(nep3, "likelihoodCovid", level = 0.95)
confint(nep3, "likelihoodFatalCovid", level = 0.95)
confint(nep3, "scale(incomeLoss)", level = 0.95)
confint(nep3, "scale(vaccinesCovid)", level = 0.95)
confint(nep3, "scale(antiVaxIndex)", level = 0.95)
confint(nep3, "scale(trustMedScientists)", level = 0.95)
confint(nep3, "scale(worriedCOVID)", level = 0.95)

stargazer(nep1, nep2, nep3,
          no.space = T,
          column.sep.width = "1pt",
          suppress.errors = T,
          font.size = "scriptsize",
          report = "vc*",
          label = "SocialExposure_NEP",
          covariate.labels = c("NEP$^z$", "Age$^d$", "Male$^d$", "Income (1000s)", "Education$^z$", "White$^d$", "Latino$^d$", 
                               "TrumpVoter$^d$", "BidenVoter$^d$", 
                               "RiskAversion$^z$", 
                               "MentalHealthIndex$^z$",
                               "PreexistCondition$^d$", 
                               "SeniorInHousehold$^d$", 
                               "SmallChildrenInHousehold$^d$", 
                               "EstLikelihoodFatalCOVID(%)",              
                               "EstLikelihoodContractingCOVID(%)",            
                               "TestedPositiveCovid$^d$",
                               "KnowSomeoneWithCOVID$^d$",
                               "LostIncomeDuringPandemic$^z$",
                               "FreqMasksInCommunity(%)",
                               "SocialGatheringInCommunity$^z$",
                               "Population Density(people/sqmi)",
                               "PastWeekCasesInState(#cases/1000pop)",
                               "PaidSickLeave$^d$",
                               "HealthInsurance$^d$",
                               "LikelihoodTakingCOVIDVaccine$^z$",
                               "AntiVaxIndex$^z$",
                               "TrustInMedicalScientists$^z$",
                               "WorriedAboutCOVID$^z$"))

nep3_1 <- lm(socialExposureIndex ~ NEP +
              age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
              scale(mentalHealthIndex)*scale(incomeLoss) +
              preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
              positiveCovid + knowCovid + 
              maskFreqCommunity + socialGatherFreqCommunity + 
              popDensity + pastWeekCasesPC +
              paidSickLeave + insured +
              scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(nep3_1)
confint(nep3_1, "scale(mentalHealthIndex):scale(incomeLoss)", level = 0.95)

nep3_2 <- lm(socialExposureIndex ~ 
             age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
             scale(mentalHealthIndex) + 
             preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
             positiveCovid + knowCovid + scale(incomeLoss) + 
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             scale(vaccinesCovid) +  scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(nep3_2)
confint(nep3_2, "scale(trustMedScientists)", level = 0.95)

# Examining whether political party moderates the relationship between NEP and social exposure
nep_dem <- lm(socialExposureIndex ~ NEP, subset = (democrat == 1), data = d)
summary(nep_dem)
confint(nep_dem, "NEP")

nep_rep <- lm(socialExposureIndex ~ NEP, subset = (republican == 1), data = d)
summary(nep_rep)
confint(nep_rep)

# Comparing NEP across Ds and Rs
nep_demrep <- lm(socialExposureIndex ~ NEP*democrat, subset = (democrat == 1 | republican == 1), data = d)
summary(nep_demrep)
confint(nep_demrep)

# Checking whether relationships hold for trump and biden voters
nep_biden <- lm(socialExposureIndex ~ NEP, subset = (biden == 1), data = d)
summary(nep_biden)
confint(nep_biden, "NEP")

nep_trump <- lm(socialExposureIndex ~ NEP, subset = (trump == 1), data = d)
summary(nep_trump)
confint(nep_trump)

nep_bidentrump <- lm(socialExposureIndex ~ NEP*biden, subset = (biden == 1 | trump == 1), data = d)
summary(nep_bidentrump)
confint(nep_bidentrump)

#### No Table: H2 NEP & Pro-sociality ####
# H2: Pro-environmental attitudes are driven, in part, by altruistic and pro-social tendencies
# Note: I only report on models 1a and 1b in the current draft of the paper, mainly for brevity.

m1a <- lm(NEP ~ sra, data = d)
m1b <- lm(NEP ~ moralObl, data = d)

m2a <- lm(NEP ~ sra + 
                 age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +
                 scale(mentalHealthIndex), 
               data = d)
m2b <- lm(NEP ~ moralObl + 
                   age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +
                   scale(mentalHealthIndex), 
                 data = d)

m3a <- lm(NEP ~ sra +
             age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
             scale(mentalHealthIndex) + 
             preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
             positiveCovid + knowCovid + scale(incomeLoss) + 
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
m3b <- lm(NEP ~ moralObl +
            age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
            scale(mentalHealthIndex) + 
            preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
            positiveCovid + knowCovid + scale(incomeLoss) + 
            maskFreqCommunity + socialGatherFreqCommunity + 
            popDensity + pastWeekCasesPC +
            paidSickLeave + insured +
            scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

summary(m1a)
summary(m2a)
summary(m3a)
confint(m1a, "sra")

summary(m1b)
summary(m2b)
summary(m3b)
confint(m1b, "moralObl")

# Interestingly, removing pandemic-related income loss restores significance for the SRA variable. 
# The SRA variable and pandemic-related income loss are highly related (beta = 41% of a standard deviation)
summary(lm(NEP ~ sra +
             age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
             scale(mentalHealthIndex) + 
             preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
             positiveCovid + knowCovid + 
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d))


#### Table 6: H3 - VSE Index & Altruism Measures
# H3: COVID-19 risk reducing behaviors are driven, in part, by altruistic and pro-social tendencies

# SRA #
sra1 <- lm(socialExposureIndex ~ sra, data = d)
sra2 <- lm(socialExposureIndex ~ sra + 
           age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +
           scale(mentalHealthIndex), 
         data = d)
sra3 <- lm(socialExposureIndex ~ sra +
                  age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                  scale(mentalHealthIndex) + 
                  preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                  positiveCovid + knowCovid + scale(incomeLoss) + 
                  maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                  popDensity + pastWeekCasesPC +
                  paidSickLeave + insured +
                  scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

summary(sra1)
confint(sra1)

summary(sra2)
confint(sra2)

summary(sra3)
confint(sra3)

summary(lm(socialExposureIndex ~ sra, data = d, subset = (republican == 1)))
summary(lm(socialExposureIndex ~ sra, data = d, subset = (democrat == 1)))
summary(lm(socialExposureIndex ~ sra + moralObl, data = d))

## Social Exposure Index ~ Moral Obligation ##
mo1 <- lm(socialExposureIndex ~ moralObl, data = d)
mo2 <- lm(socialExposureIndex ~ moralObl + 
           age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +
           scale(mentalHealthIndex), 
         data = d)
mo3 <- lm(socialExposureIndex ~ moralObl +
            age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
            scale(mentalHealthIndex) + 
            preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
            positiveCovid + knowCovid + scale(incomeLoss) + 
            maskFreqCommunity + scale(socialGatherFreqCommunity) + 
            popDensity + pastWeekCasesPC +
            paidSickLeave + insured +
            scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

mo4 <- lm(socialExposureIndex ~ sra + moralObl + 
            age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
            scale(mentalHealthIndex) + 
            preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
            positiveCovid + knowCovid + scale(incomeLoss) + 
            maskFreqCommunity + scale(socialGatherFreqCommunity) + 
            popDensity + pastWeekCasesPC +
            paidSickLeave + insured +
            scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

summary(mo1)
confint(mo1)

summary(mo2)
confint(mo2)

summary(mo3)
confint(mo3)

summary(mo4)
confint(mo4)

confint(mo2, "moralObl", level = 0.95)
confint(mo3, "moralObl", level = 0.95)
confint(mo4, "moralObl", level = 0.95)
confint(mo4, "sra", level = 0.95)

stargazer(sra2, sra3, mo2, mo3, mo4,
          no.space = T,
          column.sep.width = "-5pt",
          suppress.errors = T,
          font.size = "scriptsize",
          report = "vc*",
          omit.stat=c("f", "ser"),
          label = "SocialExposure_Altruism",
          covariate.labels = c("SRA$^z$", "MoralObligation$^z$", "Age (years)", "Male$^d$", "Income (1000s)", "Education$^z$", "Black$^d$", "Latino$^d$", 
                               "TrumpVoter$^d$", "BidenVoter$^d$", 
                               "RiskAversion$^z$", 
                               "MentalHealthIndex$^z$",
                               "PreexistCondition$^d$", 
                               "SeniorInHousehold$^d$", 
                               "SmallChildrenInHousehold$^d$", 
                               "EstLikelihoodFatalCOVID",              
                               "EstLikelihoodContractingCOVID",            
                               "TestedPositiveCovid$^d$",
                               "KnowSomeoneWithCOVID$^d$",
                               "LostIncomeDuringPandemic$^z$",
                               "FreqMasksInCommunity",
                               "SocialGatheringInCommunity$^z$",
                               "Population Density(people/sqmi)",
                               "PastWeekCasesInState(cases/1000pop)",
                               "PaidSickLeave$^d$",
                               "HealthInsurance$^d$",
                               "LikelihoodTakingCOVIDVaccine$^z$",
                               "AntiVaxIndex$^z$",
                               "TrustInMedicalScientists$^z$",
                               "WorriedAboutCOVID$^z$"))

vse_all1 <- lm(socialExposureIndex ~ age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                 scale(mentalHealthIndex) + 
                 preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                 positiveCovid + knowCovid + scale(incomeLoss) + 
                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                 popDensity + pastWeekCasesPC +
                 paidSickLeave + insured +
                 scale(vaccinesCovid) + scale(antiVaxIndex) + scale(worriedCOVID), data = d)
summary(vse_all1)
vse_all2 <- lm(socialExposureIndex ~ NEP +
                age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                scale(mentalHealthIndex) + 
                preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                positiveCovid + knowCovid + scale(incomeLoss) + 
                maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                popDensity + pastWeekCasesPC +
                paidSickLeave + insured +
                scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(vse_all2)
vse_all3 <- lm(socialExposureIndex ~ moralObl +
                 age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                 scale(mentalHealthIndex) + 
                 preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                 positiveCovid + knowCovid + scale(incomeLoss) + 
                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                 popDensity + pastWeekCasesPC +
                 paidSickLeave + insured +
                 scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(vse_all3)
vse_all4 <- lm(socialExposureIndex ~ sra + 
                age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                scale(mentalHealthIndex) + 
                preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                positiveCovid + knowCovid + scale(incomeLoss) + 
                maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                popDensity + pastWeekCasesPC +
                paidSickLeave + insured +
                scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(vse_all4)

vse_all5 <- lm(socialExposureIndex ~ NEP + moralObl + sra +
                 age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                 scale(mentalHealthIndex) + 
                 preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                 positiveCovid + knowCovid + scale(incomeLoss) + 
                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                 popDensity + pastWeekCasesPC +
                 paidSickLeave + insured +
                 scale(vaccinesCovid) + scale(antiVaxIndex) + scale(worriedCOVID), data = d)
summary(vse_all5)


vse_all6 <- lm(socialExposureIndex ~ NEP + scale(moralObl) + 
                 age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                 scale(mentalHealthIndex) + 
                 preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                 positiveCovid + knowCovid + scale(incomeLoss) + 
                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                 popDensity + pastWeekCasesPC +
                 paidSickLeave + insured +
                 scale(vaccinesCovid) + scale(antiVaxIndex) + scale(worriedCOVID), data = d)
summary(vse_all6)
vse_all_demogs <- lm(socialExposureIndex ~ NEP + scale(moralObl) + sra + 
                 age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                 scale(mentalHealthIndex) + 
                 preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                 positiveCovid + knowCovid + scale(incomeLoss) + 
                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                 popDensity + pastWeekCasesPC +
                 paidSickLeave + insured +
                 scale(vaccinesCovid) + scale(antiVaxIndex) + scale(worriedCOVID) + age + male + income + scale(education) +  black + latino + trump + biden + scale(riskAversion) + division, data = d)
summary(vse_all_demogs)
vse_all_demogs <- lm(socialExposureIndex ~ NEP + scale(moralObl) + sra + 
                       age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                       scale(mentalHealthIndex) + 
                       preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                       positiveCovid + knowCovid + scale(incomeLoss) + 
                       maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                       popDensity + pastWeekCasesPC +
                       paidSickLeave + insured +
                       scale(antiVaxIndex) + scale(worriedCOVID) + age + male + income + scale(education) +  black + latino + trump + biden + scale(riskAversion) + division, data = d)
summary(vse_all_demogs)

effect_plot(vse_all6, pred = NEP, interval = TRUE)
effect_plot(vse_all6, pred = moralObl, interval = TRUE, plot.points. = TRUE)
effect_plot(vse_all6, pred = sra, interval = TRUE, plot.points. = TRUE)
effect_plot(vse_all6, pred = antiVaxIndex, interval = TRUE, plot.points. = TRUE)
effect_plot(vse_all6, pred = socialGatherFreqCommunity, interval = TRUE, plot.points. = TRUE)

summary(lm(d$socialExpsoureIndex ~ scale(d$antiVaxIndex), data = d))



summary(lm(socialExposureIndex ~  NEP, data = d))
summary(lm(socialExposureIndex ~  scale(moralObl), data = d))
summary(lm(socialExposureIndex ~  sra, data = d))
summary(lm(socialExposureIndex ~  scale(vaccinesCovid), data = d))
summary(lm(socialExposureIndex ~  scale(antiVaxIndex), data = d))
summary(lm(socialExposureIndex ~  pastWeekCasesPC, data = d))
summary(lm(socialExposureIndex ~  NEP + scale(antiVaxIndex), data = d))
summary(lm(socialExposureIndex ~  scale(trustMedScientists), data = d))

stargazer(vse_all1, vse_all2, vse_all3, vse_all4, vse_all5,
          no.space = T,
          column.sep.width = "-5pt",
          suppress.errors = T,
          font.size = "scriptsize",
          report = "vc*",
          omit.stat=c("f", "ser"),
          label = "VariationExplained",
          covariate.labels = c("NEP$^z$", "MoralObligation$^z$", "SRA$^z$",  "Age (years)", "Male$^d$", "Income (1000s)", "Education$^z$", "Black$^d$", "Latino$^d$", 
                               "TrumpVoter$^d$", "BidenVoter$^d$", 
                               "RiskAversion$^z$", 
                               "MentalHealthIndex$^z$",
                               "PreexistCondition$^d$", 
                               "SeniorInHousehold$^d$", 
                               "SmallChildrenInHousehold$^d$", 
                               "EstLikelihoodFatalCOVID",              
                               "EstLikelihoodContractingCOVID",            
                               "TestedPositiveCovid$^d$",
                               "KnowSomeoneWithCOVID$^d$",
                               "LostIncomeDuringPandemic$^z$",
                               "FreqMasksInCommunity",
                               "SocialGatheringInCommunity$^z$",
                               "Population Density(people/sqmi)",
                               "PastWeekCasesInState(cases/1000pop)",
                               "PaidSickLeave$^d$",
                               "HealthInsurance$^d$",
                               "LikelihoodTakingCOVIDVaccine$^z$",
                               "AntiVaxIndex$^z$",
                               "TrustInMedicalScientists$^z$",
                               "WorriedAboutCOVID$^z$"))

vse_noAntiVax <- lm(socialExposureIndex ~ NEP + moralObl + sra +
                     age + male + income + scale(education) + black + latino + trump + biden + scale(riskAversion) +  
                     scale(mentalHealthIndex) + 
                     preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                     positiveCovid + knowCovid + scale(incomeLoss) + 
                     maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                     popDensity + pastWeekCasesPC +
                     paidSickLeave + insured +
                     scale(vaccinesCovid) + scale(worriedCOVID), data = d)
summary(vse_AntiVax1)


adjR2_df <- data.frame(modelAltruism = c("None", "NEP only", "MO only", "SRA only", "NEP, MO, & SRA"), 
                       adjR2 = c(.521, .532, .530, .524, .537),
                       variationExplainedAltruism = c(0, .532-.521, .530-.521, .524-.521, .537-.521)) 



#### H4: Mediation Analysis ####

mod1 <- "# a path
         moralObl ~ a * NEP

         # b path
         socialExposureIndex ~ b * moralObl

         # c prime path 
         socialExposureIndex ~ cp * NEP

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fsem1 <- sem(mod1, data = d, se = "bootstrap", bootstrap = 10000)
summary(fsem1, standardized = TRUE) 
parameterestimates(fsem1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

mod2 <- "# a path
         NEP ~ a * moralObl

         # b path
         socialExposureIndex ~ b * NEP

         # c prime path 
         socialExposureIndex ~ cp * moralObl

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fsem2 <- sem(mod2, data = d, se = "bootstrap", bootstrap = 10000)
summary(fsem2, standardized = TRUE) 
parameterestimates(fsem2, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

write_bib(x = "zipcodeR", file = "/Users/tshrum/Dropbox/Active_Papers/Bibtex/R_Packages.tex", tweak = TRUE, width = NULL, 
          prefix = getOption("knitr.bib.prefix", "R-"))

