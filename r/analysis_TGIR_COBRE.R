# TGIR-COBRE Proposal Analysis

#### Variables of Interest ####

# COVID Risk Behaviors: 
#   socialExposureIndex <- scale(scale(d$socialPeople) + scale(d$socialFreq) + d$barsW + d$visitFriendsHouseW +
#                           d$hostVisitorsW + d$party10W + d$party50W + d$indoorDiningW + d$indoorGymW)
#   riskReducingBehaviorIndex 
#   riskReductionEfficacyIndex
#   socialRiskReduction (index of 3 behaviors, only for those who see others in social contexts)

# COVID Experience: 
#   positiveCovid
#   knowCovid
#   incomeLoss

# COVID Risk Perception: 
#   riskPerceptionBehaviorIndex - how safe or unsafe does one view 23 behaviors, standardized
#   likelihoodCovid
#   likelihoodFatalCovid
#   worriedFamily

# COVID Risk: 
#   preexistCond
#   paidSickLeave
#   insured
#   household65
#   senior

# Social Norms: 
#   maskFreqCommunity
#   moralObl
# Vaccine Beliefs: vaccinesCovid, antiVaxIndex (doesn't include vaccinesCovid)
# Risk Aversion (Holt & Laury): lotterySwitchPoint, lotteryRational, lotteryMultiSwitch
# Demographics: age, male, income, education, white, latino, republican, democrat, trump, biden, region
# Trust: trust (combination of trust level in 9 types of today's leaders)


# How do risk behaviors relate to one another?

summary(lm(socialExposureIndex ~ riskReducingBehaviorIndex + riskReductionEfficacyIndex + 
             riskPerceptionBehaviorIndex, data = d))

# How do risk perceptions relate to risk behavior?
summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex, data = d))
summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex + poly(likelihoodCovid, 2) +
             poly(likelihoodFatalCovid, 2) + worriedFamily, data = d))
summary(lm(socialExposureIndex ~ poly(likelihoodCovid, 2) +
              worriedFamily, data = d))
summary(lm(socialExposureIndex ~ likelihoodCovid, data = d))
summary(lm(socialExposureIndex ~ poly(likelihoodCovid, 2, raw = TRUE), data = d))
summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex*poly(likelihoodCovid, 2), data = d))

summary(lm(riskPerceptionBehaviorIndex ~ pastWeekCasesPC, data = d))
summary(lm(likelihoodCovid ~ pastWeekCasesPC, data = d))  # no relationship between perceived risk and actual cases
summary(lm(likelihoodFatalCovid ~ pastWeekDeathsPC + trump + age, data = d))  # 
summary(lm(riskPerceptionBehaviorIndex ~ pastWeekCasesEstimatePC, data = d))
summary(lm(likelihoodCovid ~ pastWeekCasesEstimatePC, data = d))  # no relationship between perceived risk and actual cases
summary(lm(likelihoodFatalCovid ~ pastWeekDeathsEstimatePC + trump + age, data = d))  # 

summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE), data = d))
summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + preexistCond*age, data = d))
summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + preexistCond*age +
             trustMedScientists + trump, data = d))

summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + preexistCond + age +
             trustMedScientists, data = d, subset = (trump == 1)))
summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + preexistCond + age +
             trustMedScientists, data = d, subset = (trump == 0)))

summary(lm(riskPerceptionBehaviorIndex ~  poly(likelihoodCovid, 2, raw = TRUE) + preexistCond + senior + worriedFamily + education +
             lotterySwitchPoint + prayEff + trump, data = d))

summary(lm(riskPerceptionBehaviorIndex ~  poly(likelihoodCovid, 2, raw = TRUE) + preexistCond + senior + worriedFamily + education +
             lotterySwitchPoint + pastWeekCases + trump, data = d))

summary(lm(riskPerceptionBehaviorIndex ~ pastWeekCasesPC + pastWeekDeathsPC +
             pastWeekCasesEstimatePC + pastWeekDeathsEstimatePC, data = d))

summary(lm(riskPerceptionBehaviorIndex ~ pastWeekCasesEstimatePC + pastWeekDeathsEstimatePC + 
             trustMedScientists + trump + mentalDepressed, data = d))
summary(lm(socialExposureIndex ~ pastWeekCasesEstimatePC + pastWeekDeathsEstimatePC + trustMedScientists, data = d))
summary(lm(socialExposureIndex ~ pastWeekCasesPC + pastWeekDeathsPC + trustMedScientists, data = d))
summary(lm(likelihoodFatalCovid ~ pastWeekCasesEstimatePC + pastWeekDeathsEstimatePC + 
             trustMedScientists + trump + mentalDepressed, data = d))
summary(lm(socialExposureIndex ~ pastWeekCasesEstimatePC + caseEstimateError + worriedFamily, data = d))
summary(lm(riskPerceptionBehaviorIndex ~ pastWeekCasesEstimatePC + caseEstimateError + worriedFamily, data = d))
summary(lm(likelihoodCovid ~ pastWeekCasesEstimatePC + caseEstimateError, data = d))

d1 <- d[!is.na(d$riskPerceptionBehaviorIndex),]
d1 <- d1[!is.na(d1$riskReductionEfficacyIndex),]
summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex + lotterySwitchPoint + riskReductionEfficacyIndex +
             trump + pod + socializedOutdoors  + sra +
             household65 + age + trustMedScientists + preexistCond*age + insured +
             likelihoodFatalCovid + positiveCovid + incomeLoss + knowCovid + NEP, data = d1))

summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex + lotterySwitchPoint + riskReductionEfficacyIndex +
             trump + pod + socializedOutdoors  + sra +
             household65 + age + trustMedScientists + preexistCond*age + insured +
             likelihoodCovid + positiveCovid + incomeLoss + knowCovid + NEP +
             income + education, data = d1, subset = (d1$positiveCovid == 0)))

summary(lm(riskPerceptionBehaviorIndex ~ knowCovid + incomeLoss + trustMedScientists, data = d1,
           subset = (positiveCovid == 0)))

summary(lm(likelihoodCovid ~ trump, data = d1))

summary(lm(socialExposureIndex ~ mentalNervous, data = d1))
summary(lm(socialExposureIndex ~ mentalWorrying, data = d1))
summary(lm(socialExposureIndex ~ mentalDepressed, data = d1))
summary(lm(socialExposureIndex ~ mentalApathy, data = d1))
summary(lm(socialExposureIndex ~ mentalLossControl, data = d1))
summary(lm(socialExposureIndex ~ mentalProblems, data = d1))
summary(lm(socialExposureIndex ~ mentalGoingYourWay, data = d1))
summary(lm(socialExposureIndex ~ mentalOverwhelmed, data = d1))

summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex + lotterySwitchPoint + riskReductionEfficacyIndex +
             trump + pod + socializedOutdoors  + sra +
             household65 + age + trustMedScientists + preexistCond*age + insured +
             likelihoodCovid + incomeLoss + knowCovid + NEP +
             income + education + mentalDepressed, data = d1, subset = (d1$positiveCovid == 0)))

summary(lm(riskPerceptionBehaviorIndex ~ mentalDepressed, data = d1))

summary(lm(mentalDepressed ~ riskPerceptionBehaviorIndex + lotterySwitchPoint + riskReductionEfficacyIndex +
             trump + pod + socializedOutdoors  + sra +
             household65 + age + trustMedScientists + preexistCond*age + insured +
             likelihoodCovid + incomeLoss + knowCovid + NEP +
             income + education, data = d1))

summary(lm(mentalDepressed ~ poly(likelihoodCovid, 1, raw = T), data = d))


# Mediation model for likelihood of covid 
summary(lm(socialExposureIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + riskPerceptionBehaviorIndex, data = d))
summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) , data = d))



# Plotting the relationship between personal estimation of the likelihood of getting COVID and key risk variables
summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE), data = d, subset = (positiveCovid == 0)))
summary(lm(socialExposureIndex ~ poly(likelihoodCovid, 2, raw = TRUE), data = d, subset = (positiveCovid == 0)))
summary(lm(riskReductionEfficacyIndex ~ poly(likelihoodCovid, 2, raw = TRUE), data = d, subset = (positiveCovid == 0)))


summary(lm(riskReductionEfficacyIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + trump +
             + worriedFamily + preexistCond + 
             positiveCovid + knowCovid +
             moralObl + 
             insured +
              trump  +
             antiVaxIndex + workFromHomeDays , data = d, subset = (positiveCovid == 0)))
m <- NULL

d1 <- d[!is.na(d$riskPerceptionBehaviorIndex) & !is.na(d$likelihoodCovid) & d$positiveCovid == 0 &
          !is.na(d$worriedFamily) & !is.na(d$antiVaxIndex) & !is.na(d$moralObl) & !is.na(d$preexistCond),]
predict(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + trump +
             + worriedFamily + preexistCond + 
             positiveCovid + knowCovid +
             moralObl + 
             insured +
             trump  +
             antiVaxIndex + workFromHomeDays , data = d, subset = (positiveCovid == 0)), d) -> m$yhat
unlist(m$yhat) -> x

predict(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + trump +
             + worriedFamily + preexistCond + 
             positiveCovid + knowCovid +
             moralObl + 
             insured +
             trump  +
             antiVaxIndex + workFromHomeDays , data = d, subset = (positiveCovid == 0)), d) -> m$yhat
unlist(m$yhat) -> x

modelFit <- data.frame(predict = x, likelihoodCovid = d$likelihoodCovid, riskPerceptionBehaviorIndex = d$riskPerceptionBehaviorIndex)

ggplot(data = d, aes(x = d$likelihoodCovid, y = d$riskPerceptionBehaviorIndex)) +
  geom_point() + geom_line(aes(x = d$likelihoodCovid, y = modelFit$predict))

ggplot(data = modelFit, aes(x = likelihoodCovid, y = predict)) +
  geom_point() +
  geom_smooth() +
  xlab("Subjective Estimated Likelihood of Contracting COVID-19") +
  ylab("Risk Perception of Behavior Index")
 
ggplot(data = d, aes(x = d$likelihoodCovid, y = d$socialExposureIndex)) +
  geom_point()

summary(lm(riskPerceptionBehaviorIndex ~ poly(NEP, 2, raw = TRUE) + trump, data = d))
summary(lm(riskPerceptionBehaviorIndex ~ poly(NEP, 1, raw = TRUE), data = d))

hist(d$NEP)
summary(d$NEP)

d$enviro <- ifelse(d$NEP >= 16, 1, 0)

summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + trump + republican + education, data = d, subset = (positiveCovid == 0 & enviro == 1)))

summary(lm(riskPerceptionBehaviorIndex ~ poly(likelihoodCovid, 2, raw = TRUE) + trump + republican + education, data = d, subset = (positiveCovid == 0 & enviro == 0)))

summary(lm(likelihoodCovid ~ c))

ggplot() +
  geom_density(data = d, aes(x = likelihoodCovid, group = enviro, fill = enviro), alpha = 0.5)

ggplot() +
  geom_density(data = d, aes(x = likelihoodCovid, group = trump, fill = trump), alpha = 0.5)


