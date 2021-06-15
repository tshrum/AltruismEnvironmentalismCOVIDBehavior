# Analysis for Pro-Environmental Attitudes Paper

load("data/prepped.R")

d %>% filter(invalidResponse == 0) -> d

source('r/dataCheck.R', echo=TRUE)

varsOrder <- c("NEP", "age", "male", "income", "education", "white", "latino", "trump", "biden",
               "lotterySwitchPoint",
               "mentalHealthIndex",
               "likelihoodCovid", "preexistCond", "household65", "positiveCovid", 
               "knowCovid", "maskFreqCommunity", 
               "incomeLoss", 
               "paidSickLeave", "insured",
               "vaccinesCovid", "antiVaxIndex", "worriedCOVID")


#### Table 4: Looking at correlates of NEP and SRA ####
m1 <- lm(NEP ~ age + male + income + education + white + latino + trump + biden + scale(lotterySwitchPoint) + division, data = d)
m2 <- lm(sra ~ age + male + income + education + white + latino + trump + biden + scale(lotterySwitchPoint) + division, data = d)
m3 <- lm(scale(moralObl) ~ age + male + income + education + white + latino + trump + biden + scale(lotterySwitchPoint) + division, data = d)

summary(m1)
summary(m2)
summary(m3)
stargazer(m1, m2, m3,
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
linearHypothesis(m2, "trump = biden")
linearHypothesis(m1, "trump = biden")
linearHypothesis(m3, "trump = biden")



#### Table 5: H1 VSE Index & NEP ####
# H1: People with pro-environmental attitudes have a higher willingness to adhere to COVID risk mitigation recommendations compared to those with less pro-environmental attitudes.

# Social Exposure Index ~ NEP #
nep1 <- lm(socialExposureIndex ~ NEP, data = d)
nep2 <- lm(socialExposureIndex ~ NEP + 
           age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +
           scale(mentalHealthIndex), 
         data = d)
nep3 <- lm(socialExposureIndex ~ NEP +
           age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
           scale(mentalHealthIndex) + 
           preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
           positiveCovid + knowCovid + scale(incomeLoss) + 
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
summary(nep3)

confint(nep1, "NEP", level = 0.95)
confint(nep2, "NEP", level = 0.95)
confint(nep3, "NEP", level = 0.95)

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


summary(lm(socialExposureIndex ~ NEP, subset = (republican == 1), data = d))
summary(lm(socialExposureIndex ~ NEP, subset = (democrat == 1), data = d))
summary(lm(socialExposureIndex ~ NEP*republican, data = d, subset = (republican == 1 | democrat == 1)))
summary(lm(socialExposureIndex ~ NEP*trump, data = d, subset = (trump == 1 | biden == 1)))
summary(lm(socialExposureIndex ~ NEP*democrat, data = d))

#### No Table: H2 NEP & Pro-sociality ####
# H2: Pro-environmental attitudes are driven, in part, by altruistic and pro-social tendencies
# Note: I only report on models 1a and 1b in the current draft of the paper, mainly for brevity.

m1a <- lm(NEP ~ sra, data = d)
m1b <- lm(NEP ~ moralObl, data = d)

m2a <- lm(NEP ~ sra + 
                 age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +
                 scale(mentalHealthIndex), 
               data = d)
m2b <- lm(NEP ~ moralObl + 
                   age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +
                   scale(mentalHealthIndex), 
                 data = d)

m3a <- lm(NEP ~ sra +
             age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
             scale(mentalHealthIndex) + 
             preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
             positiveCovid + knowCovid + scale(incomeLoss) + 
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)
m3b <- lm(NEP ~ moralObl +
            age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
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
             age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
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
           age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +
           scale(mentalHealthIndex), 
         data = d)
sra3 <- lm(socialExposureIndex ~ sra +
                  age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
                  scale(mentalHealthIndex) + 
                  preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
                  positiveCovid + knowCovid + scale(incomeLoss) + 
                  maskFreqCommunity + socialGatherFreqCommunity + 
                  popDensity + pastWeekCasesPC +
                  paidSickLeave + insured +
                  scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

summary(sra1)
summary(sra2)
summary(sra3)

confint(sra2, "sra", level = 0.95)
confint(sra3, "sra", level = 0.95)

summary(lm(socialExposureIndex ~ sra, data = d, subset = (republican == 1)))
summary(lm(socialExposureIndex ~ sra, data = d, subset = (democrat == 1)))
summary(lm(socialExposureIndex ~ sra + moralObl, data = d))

## Social Exposure Index ~ Moral Obligation ##
mo1 <- lm(socialExposureIndex ~ moralObl, data = d)
mo2 <- lm(socialExposureIndex ~ moralObl + 
           age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +
           scale(mentalHealthIndex), 
         data = d)
mo3 <- lm(socialExposureIndex ~ moralObl +
            age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
            scale(mentalHealthIndex) + 
            preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
            positiveCovid + knowCovid + scale(incomeLoss) + 
            maskFreqCommunity + socialGatherFreqCommunity + 
            popDensity + pastWeekCasesPC +
            paidSickLeave + insured +
            scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

mo4 <- lm(socialExposureIndex ~ sra + moralObl + 
            age + male + income + scale(education) + white + latino + trump + biden + scale(lotterySwitchPoint) +  
            scale(mentalHealthIndex) + 
            preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
            positiveCovid + knowCovid + scale(incomeLoss) + 
            maskFreqCommunity + socialGatherFreqCommunity + 
            popDensity + pastWeekCasesPC +
            paidSickLeave + insured +
            scale(vaccinesCovid) + scale(antiVaxIndex) + scale(trustMedScientists) + scale(worriedCOVID), data = d)

summary(mo1)
summary(mo2)
summary(mo3)
summary(mo4)

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
          covariate.labels = c("SRA", "MoralObligation", "Age", "Male", "Income", "Education", "White", "Latino", 
                               "TrumpVoter", "BidenVoter", 
                               "RiskAversion", 
                               "MentalHealthIndex",
                               "PreexistCondition", 
                               "SeniorInHousehold", 
                               "SmallChildrenInHousehold", 
                               "EstLikelihoodFatalCOVID",              
                               "EstLikelihoodContractingCOVID",            
                               "TestedPositiveCovid",
                               "KnowSomeoneWithCOVID",
                               "LostIncomeDuringPandemic",
                               "FreqMasksInCommunity",
                               "SocialGatheringInCommunity",
                               "Population Density",
                               "PastWeekCasesInState",
                               "PaidSickLeave",
                               "HealthInsurance",
                               "LikelihoodTakingCOVIDVaccine",
                               "AntiVaxIndex",
                               "TrustInMedicalScientists",
                               "WorriedAboutCOVID"))

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

with(d, mediation(x = NEP, mediator = moralObl, dv = socialExposureIndex, bootstrap = TRUE, which.boot = "BCa", B = 10000))
with(d, mediation(x = moralObl, mediator = NEP, dv = socialExposureIndex, bootstrap = TRUE, which.boot = "BCa", B = 10000))



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

mod2 <- "# a path
         sra ~ a * NEP

         # b path
         socialExposureIndex ~ b * sra

         # c prime path 
         socialExposureIndex ~ cp * NEP

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fsem2 <- sem(mod2, data = d, se = "bootstrap", bootstrap = 10000)
summary(fsem2, standardized = TRUE) 

parameterestimates(fsem1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable()

mod2 <- "# a path
         sra ~ a * NEP

         # b path
         socialExposureIndex ~ b * sra

         # c prime path 
         socialExposureIndex ~ cp * NEP

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fsem2 <- sem(mod2, data = d, se = "bootstrap", bootstrap = 10000)
summary(fsem2, standardized = TRUE) 


summary(lm(NEP ~ moralObl, data = d))
  
with(d, mediation(x = NEP, mediator = moralObl, dv = socialExposureIndex, bootstrap = TRUE, which.boot = "BCa", B = 10000))
  
with(d, mediation(x = moralObl, mediator = NEP, dv = socialExposureIndex, bootstrap = TRUE, which.boot = "BCa", B = 10000))
with(d, mediation(x = worriedCOVID, mediator = NEP, dv = socialExposureIndex, bootstrap = TRUE, which.boot = "BCa", B = 10000))


write_bib(x = "zipcodeR", file = "/Users/tshrum/Dropbox/Active_Papers/Bibtex/R_Packages.tex", tweak = TRUE, width = NULL, 
          prefix = getOption("knitr.bib.prefix", "R-"))


#### Back Parking Lot ####

# pulling 
d %>%
  dplyr::select(socialExposureIndex, NEP,
                age, male, income, education, white, latino, trump, biden, lotterySwitchPoint,  
                mentalHealthIndex, 
                preexistCond, household65, smallChildren, likelihoodFatalCovid, likelihoodCovid,
                positiveCovid, knowCovid, incomeLoss, 
                maskFreqCommunity, socialGatherFreqCommunity, 
                popDensity, pastWeekCasesPC,
                paidSickLeave, insured,
                vaccinesCovid, antiVaxIndex, trustMedScientists, worriedCOVID) -> d1
d1[complete.cases(d1),] -> d2

summary(lm(sraNAremoved ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             lotterySwitchPoint + trump + biden, data = d))
summary(lm(sraNAomittedInMean ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             lotterySwitchPoint + trump + biden, data = d))
summary(lm(sra ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             lotterySwitchPoint + trump + biden, data = d))



summary(lm(NEP ~ worriedCOVID + moralObl*sra, data = d))
m3 <- lm(NEP ~ sra + moralObl + worriedCOVID + age + male + income + education + white + latino + trump + biden +
           lotterySwitchPoint, data = d)
m4 <- lm(socialExposureIndex ~ NEP +  
           age + male + income + education + white + latino + trump + biden + popDensity + 
           likelihoodCovid + preexistCond + household65 + smallChildren +
           positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           lotterySwitchPoint +  
           vaccinesCovid + antiVaxIndex + trustMedScientists +
           worriedCOVID + moralObl + sra, data = d)

summary(m1)
summary(m2)
summary(m3)


m4 <- lm(socialExposureIndex ~ NEP +  
           age + male + income + education + white + latino + trump + biden + popDensity + 
           likelihoodCovid + preexistCond + household65 + smallChildren +
           positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           lotterySwitchPoint +  
           vaccinesCovid + antiVaxIndex + trustMedScientists +
           worriedCOVID + moralObl + sra, data = d)



## Social Exposure Index ~ NEP + Moral Obligation ##
m1 <- lm(socialExposureIndex ~ NEP + moralObl, data = d)
summary(m1)
m2 <- lm(socialExposureIndex ~ NEP + moralObl + 
           age + male + income + education + white + latino + trump + biden + lotterySwitchPoint +
           mentalHealthIndex, 
         data = d)
m3 <- lm(socialExposureIndex ~ NEP + moralObl +
           age + male + income + education + white + latino + trump + biden + lotterySwitchPoint +  
           preexistCond + household65 + smallChildren + likelihoodFatalCovid + likelihoodCovid +
           positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           vaccinesCovid + antiVaxIndex + trustMedScientists + worriedCOVID, data = d)
stargazer(m1, m2, m3,
          no.space = T,
          column.sep.width = "1pt",
          suppress.errors = T,
          font.size = "scriptsize",
          report = "vc*",
          omit.stat=c("f", "ser"))



summary(lm(socialExposureIndex ~ division + pastWeekCasesPC, data = d))


summary(lm(socialExposureIndex ~ likelihoodCovid*likelihoodFatalCovid, data = d))

summary(lm(socialExposureIndex ~ NEP +  
             age + male + income + education + white + latino + trump + biden + 
             popDensity + 
             preexistCond + household65 +
             positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
             maskFreqCommunity + socialGatherFreqCommunity +
             paidSickLeave + insured +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex + pastWeekCasesPC +
             trustMedScientists + smallChildren + republican + democrat, data = d))

summary(lm(moralObl ~ riskPerceptionBehaviorIndex + NEP + riskReductionEfficacyIndex , data = d))


## Looking at individual risk mitigation activities. The indices do not work. There are different
## types of behavior that go in opposite directions. There are also different underlying reasons
## for answering a certain way on a question (like avoiding air travel). 

# Pro-environmental attitudes positively correlate with these behaviors:
summary(glm(wornMask ~ NEP, data = d, family = "binomial"))
summary(glm(washHands ~ NEP, data = d, family = "binomial"))
summary(glm(avoidedHighRiskPeople ~ NEP, data = d, family = "binomial"))
summary(glm(avoidedPublicSpaces ~ NEP, data = d, family = "binomial"))
summary(glm(avoidedRestaurants ~ NEP, data = d, family = "binomial"))
summary(glm(stayedHome ~ NEP, data = d, family = "binomial"))

# Pro-environmental attitudes have no relationship with these behaviors:
summary(glm(workedHome ~ NEP, data = d, family = "binomial"))
summary(glm(outdoorExercise ~ NEP, data = d, family = "binomial"))

# Pro-environmental attitudes negatively correlate with these behaviors:
summary(glm(postponedWorkTravel ~ NEP, data = d, family = "binomial"))
summary(glm(postponedVacationTravel~ NEP, data = d, family = "binomial"))
summary(glm(postponedWorkSchoolActivities ~ NEP, data = d, family = "binomial"))
summary(glm(postponedSocialActivities ~ NEP, data = d, family = "binomial"))
summary(glm(visitedDoctor ~ NEP, data = d, family = "binomial"))
summary(glm(postponedDoctor ~ NEP, data = d, family = "binomial"))
summary(glm(stockpiledFoodWater ~ NEP, data = d, family = "binomial"))
summary(glm(prayed ~ NEP, data = d, family = "binomial"))
summary(glm(stockpiledHandSanitizer ~ NEP, data = d, family = "binomial"))
summary(glm(stockpiledMeds ~ NEP, data = d, family = "binomial"))
summary(glm(socializedOutdoors ~ NEP, data = d, family = "binomial"))
summary(glm(avoidedSchoolDaycare ~ NEP, data = d, family = "binomial"))
summary(glm(quarantined ~ NEP, data = d, family = "binomial"))
summary(glm(pod ~ NEP, data = d, family = "binomial"))


# moralObl
summary(glm(wornMask ~ moralObl, data = d, family = "binomial"))
summary(glm(washHands ~ moralObl, data = d, family = "binomial"))
summary(glm(avoidedHighRiskPeople ~ moralObl, data = d, family = "binomial"))
summary(glm(avoidedPublicSpaces ~ moralObl, data = d, family = "binomial"))
summary(glm(avoidedRestaurants ~ moralObl, data = d, family = "binomial"))
summary(glm(stayedHome ~ moralObl, data = d, family = "binomial"))
summary(glm(workedHome ~ moralObl, data = d, family = "binomial"))
summary(glm(outdoorExercise ~ moralObl, data = d, family = "binomial"))
summary(glm(postponedWorkTravel ~ moralObl, data = d, family = "binomial"))
summary(glm(postponedVacationTravel~ moralObl, data = d, family = "binomial"))
summary(glm(postponedWorkSchoolActivities ~ moralObl, data = d, family = "binomial"))
summary(glm(postponedSocialActivities ~ moralObl, data = d, family = "binomial"))
summary(glm(visitedDoctor ~ moralObl, data = d, family = "binomial"))
summary(glm(postponedDoctor ~ moralObl, data = d, family = "binomial"))
summary(glm(stockpiledFoodWater ~ moralObl, data = d, family = "binomial"))
summary(glm(prayed ~ moralObl, data = d, family = "binomial"))
summary(glm(stockpiledHandSanitizer ~ moralObl, data = d, family = "binomial"))
summary(glm(stockpiledMeds ~ moralObl, data = d, family = "binomial"))
summary(glm(socializedOutdoors ~ moralObl, data = d, family = "binomial"))
summary(glm(avoidedSchoolDaycare ~ moralObl, data = d, family = "binomial"))
summary(glm(quarantined ~ moralObl, data = d, family = "binomial"))
summary(glm(pod ~ moralObl, data = d, family = "binomial"))

# SRA
summary(glm(wornMask ~ sra, data = d, family = "binomial"))
summary(glm(washHands ~ sra, data = d, family = "binomial"))
summary(glm(avoidedHighRiskPeople ~ sra, data = d, family = "binomial"))
summary(glm(avoidedPublicSpaces ~ sra, data = d, family = "binomial"))
summary(glm(avoidedRestaurants ~ sra, data = d, family = "binomial"))
summary(glm(stayedHome ~ sra, data = d, family = "binomial"))
summary(glm(workedHome ~ sra, data = d, family = "binomial"))
summary(glm(outdoorExercise ~ sra, data = d, family = "binomial"))
summary(glm(postponedWorkTravel ~ sra, data = d, family = "binomial"))
summary(glm(postponedVacationTravel~ sra, data = d, family = "binomial"))
summary(glm(postponedWorkSchoolActivities ~ sra, data = d, family = "binomial"))
summary(glm(postponedSocialActivities ~ sra, data = d, family = "binomial"))
summary(glm(visitedDoctor ~ sra, data = d, family = "binomial"))
summary(glm(postponedDoctor ~ sra, data = d, family = "binomial"))
summary(glm(stockpiledFoodWater ~ sra, data = d, family = "binomial"))
summary(glm(prayed ~ sra, data = d, family = "binomial"))
summary(glm(stockpiledHandSanitizer ~ sra, data = d, family = "binomial"))
summary(glm(stockpiledMeds ~ sra, data = d, family = "binomial"))
summary(glm(socializedOutdoors ~ sra, data = d, family = "binomial"))
summary(glm(avoidedSchoolDaycare ~ sra, data = d, family = "binomial"))
summary(glm(quarantined ~ sra, data = d, family = "binomial"))
summary(glm(pod ~ sra, data = d, family = "binomial"))




#### H1.3: COVID risk reducing behaviors are driven, in part, by altruistic and pro-social tendencies

summary(lm(socialExposureIndex ~ sra, data = d))
summary(lm(socialExposureIndex ~ NEP +  
             age + male + income + education + white + latino + trump + biden + popDensity + 
             likelihoodCovid + preexistCond + household65 + smallChildren +
             positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex + trustMedScientists +
             worriedCOVID + moralObl + sra, data = d))

summary(lm(socialExposureIndex ~ 
             age + male + income + education + white + latino + trump + biden + popDensity + 
             likelihoodCovid + preexistCond + household65 + smallChildren +
             positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex + trustMedScientists +
             worriedCOVID + moralObl + sra + tightness, data = d))

summary(lm(pastWeekCasesPC ~ tightness + trumpPercent, data = d))
summary(lm(pastWeekCasesPC ~ tightness, data = d))
summary(lm(socialExposureIndex ~ tightness + trump, data = d))
summary(lm(socialExposureIndex ~ tightness + pastWeekCasesPC, data = d))
summary(lm(sra ~ tightness + age + male + income + education + white + latino + trump + biden + popDensity, data = d))
