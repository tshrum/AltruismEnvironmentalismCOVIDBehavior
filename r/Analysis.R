# Analysis
# Created by: Trisha R. Shrum


#### Variables of Interest ####
# Pro-environmental: NEP

# Altruism: sra

# COVID Risk Behaviors: 
#   socialExposureIndex <- scale(scale(d$socialPeople) + scale(d$socialFreq) + d$barsW + d$visitFriendsHouseW +
#                           d$hostVisitorsW + d$party10W + d$party50W + d$indoorDiningW + d$indoorGymW)
#
#   riskReducingBehaviorIndex: standardized sum of yes/no questions about the following behaviors over the past 7 days:
#       washed hands or used hand santizer several times per day, cancelled/postponed air travel for work,
#       cancelled/postponed air travel for pleasure, cancelled/postponed work/school activities, 
#       cancelled a doctors appt, stockpiled food/water, avoided contact with high-risk people,
#       stockpiled hand sanitizer, worked or studied at home, worn a mask or other face covering, 
#       stockpiled medication, held social activities outdoors instead of indoors, kept children out of school/daycare,
#       Remained in your residence at all times, except for essential activities or exercise,
#       Joined with or maintained a pod with another household for exclusive non-distanced socializing
#
#   riskReducingBehaviorIndex1: standardized sum of yes/no questions about the following behaviors over the past 7 days:
#       washed hands or used hand santizer several times per day, cancelled/postponed air travel for work,
#       cancelled/postponed air travel for pleasure, cancelled/postponed work/school activities, 
#       cancelled a doctors appt, stockpiled food/water, avoided contact with high-risk people,
#       stockpiled hand sanitizer, worked or studied at home, worn a mask or other face covering, 
#       stockpiled medication, held social activities outdoors instead of indoors, kept children out of school/daycare,
#       Remained in your residence at all times, except for essential activities or exercise,
#       Joined with or maintained a pod with another household for exclusive non-distanced socializing

scale(x$Q6_1 + x$Q6_2 + x$Q6_3 + x$Q6_4 + x$Q6_5 + x$Q6_7 + x$Q6_8 + x$Q6_9 + 
x$Q6_11 + x$Q6_13 + x$Q6_14 + x$Q6_15 + x$Q6_16 + x$Q6_17 + x$Q6_18 + d$quarantined +
  d$stayedHome + d$pod)
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


summary(lm(socialExposureIndex ~ NEP, data = d))
summary(lm(NEP ~ riskPerceptionBehaviorIndex + lotterySwitchPoint + riskReductionEfficacyIndex +
             trump + pod + socializedOutdoors  + sra +
             household65 + age + trustMedScientists + preexistCond*age + insured +
             likelihoodFatalCovid + positiveCovid + incomeLoss + knowCovid +
             income + education + male + republican + democrat, data = d))

summary(lm(socialRiskReduction))

summary(lm(socialExposureIndex ~ riskPerceptionBehaviorIndex, data = d))

summary(lm(socialExposureIndex ~ preexistCond + senior + household65 + age, data = d))
summary(lm(socialExposureIndex ~ preexistCond + senior + household65 + age + male + income + education + 
             white + latino + trump, data = d))

summary(lm(socialExposureIndex ~ trust + senior + household65 + age + male + income + education + 
             white + latino + trump, data = d))
summary(lm(socialExposureIndex ~ trustMedia + senior + household65 + age + male + income + education + 
             white + latino + trump, data = d))
summary(lm(socialExposureIndex ~ trust, data = d))
summary(lm(socialExposureIndex ~ d$trustPoliticians + d$trustBusinessLeaders + d$trustDoctors + d$trustMedia +
             d$trustMedScientists + d$trustMilitary + d$trustReligiousLeaders + d$trustSchoolLeaders +
             d$trustScientists + age + male + income + education + 
             white + latino + trump, data = d))

summary(lm(riskReducingBehaviorIndex ~ trust, data = d))
summary(lm(riskReducingBehaviorIndex ~ d$trustPoliticians + d$trustBusinessLeaders + d$trustDoctors + d$trustMedia +
             d$trustMedScientists + d$trustMilitary + d$trustReligiousLeaders + d$trustSchoolLeaders +
             d$trustScientists + age + male + income + education + 
             white + latino + trump, data = d))

summary(lm(riskReductionEfficacyIndex ~ trust, data = d))
summary(lm(riskReductionEfficacyIndex ~ d$trustPoliticians + d$trustBusinessLeaders + d$trustDoctors + d$trustMedia +
             d$trustMedScientists + d$trustMilitary + d$trustReligiousLeaders + d$trustSchoolLeaders +
             d$trustScientists, data = d))

summary(lm(socialExposureIndex ~ washHands + wornMask + socializedOutdoors +
             age + male + income + education + 
             white + latino + trump, data = d))

summary(lm(riskReducingBehaviorIndex ~ riskReductionEfficacyIndex, data = d))

# Examining risk reducing behaviors for issues. I think we are measuring a lot of different things here.
summary(glm(washHands ~ age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))
summary(glm(postponedWorkTravel ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(postponedVacationTravel ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(postponedWorkSchoolActivities ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(postponedSocialActivities ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(visitedDoctor  ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(postponedDoctor ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(stockpiledFoodWater ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(avoidedHighRiskPeople ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(avoidedPublicSpaces ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(prayed ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(avoidedRestaurants ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(stockpiledHandSanitizer ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(workedHome ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(wornMask ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(stockpiledMeds ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(socializedOutdoors ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(avoidedSchoolDaycare ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(quarantined  ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?
summary(glm(stayedHome ~  age + male + income + education + trustMedScientists, family = binomial(link = "logit"), data = d))  # Remove from index?





#### Risk Taking Behavior Index ####
# Demographics only
summary(lm(riskTakingBehaviorIndex ~ age + male + income + education + region, data = d))

# Kitchen Sink Model
summary(lm(riskTakingBehaviorIndex ~ riskReducingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))

# Kitchen sink model, limited to people who have not tested positive for COVID
summary(lm(riskTakingBehaviorIndex ~ riskReducingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex +
             NEP, data = d, subset = positiveCovid == 0))

# Kitchen Sink Model, taking out other indices to avoid multicolinearity with other variables
summary(lm(riskTakingBehaviorIndex ~
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino + trump + region + biden +
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))





# Adjusted R2 Maximizing Model
# Adjusted R-squared:  0.5393
rtb <- lm(riskTakingBehaviorIndex ~ riskReducingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + 
             paidSickLeave + insured +
             male + income + trump + 
             lotterySwitchPoint + 
             vaccinesCovid + antiVaxIndex +
             NEP, data = d)
summary(rtb)

rtb <- lm(riskTakingBehaviorIndex ~ 
            worriedFamily + preexistCond + 
            positiveCovid + knowCovid + incomeLoss +
            insured +
            trump + 
            vaccinesCovid + antiVaxIndex +
            NEP, data = d)
summary(rtb)

# Adjusted R2 Maximizing Model, limited to people who have not tested positive
# Adjusted R-squared:  0.5122 
summary(lm(riskTakingBehaviorIndex ~ riskReducingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + worriedFamily + preexistCond + 
             knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + 
             paidSickLeave + insured +
             male + income + trump + 
             lotterySwitchPoint + 
             vaccinesCovid + antiVaxIndex +
             NEP, data = d, subset = positiveCovid == 0))

# Adj R2 maximizing model without other behavioral indices
summary(lm(riskTakingBehaviorIndex ~ 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + 
             paidSickLeave + insured +
             income + trump  +
             lotterySwitchPoint + 
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))


# Reducing the model to ~0.05 variables (reduced stepwise, this is the final result)
summary(lm(riskTakingBehaviorIndex ~ likelihoodCovid + worriedFamily + preexistCond + 
             knowCovid + incomeLoss +
             moralObl + 
             paidSickLeave + insured +
             trump +
             vaccinesCovid + antiVaxIndex +
             NEP, data = d, subset = positiveCovid == 0))

#### Risk Reducing Behavior Index ####

# Kitchen Sink Model
summary(lm(riskReducingBehaviorIndex ~ riskTakingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino  + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))

# Adj R2 Maximizing Model
# Adjusted R-squared:  0.5853 
rrb <- lm(riskReducingBehaviorIndex ~ riskTakingBehaviorIndex + riskReductionEfficacyIndex + 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             incomeLoss +
             moralObl + 
             paidSickLeave +
             education + white + trump + region + biden +
             lotterySwitchPoint + 
             antiVaxIndex +
             NEP, data = d)
summary(rrb)


#### Risk Reduction Efficacy Beliefs Index ####
# We need to bring in other trust variables here. I probably should bring those in for the other DVs as well...

# Kitchen Sink Model
summary(lm(riskReductionEfficacyIndex ~ riskReducingBehaviorIndex + riskTakingBehaviorIndex + 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino  + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))

# Kitchen Sink Model, without other behavioral indices
summary(lm(riskReductionEfficacyIndex ~ 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + 
             paidSickLeave + insured +
             age + male + income + education + white + latino + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex +
             NEP, data = d))

# Maximized Adj R2, without other behavioral indices
# Adjusted R-squared:  0.2863 
rre <- lm(riskReductionEfficacyIndex ~ 
             likelihoodCovid + likelihoodFatalCovid + worriedFamily + 
             knowCovid +
             maskFreqCommunity + moralObl + 
             age + education + biden +
             lotterySwitchPoint +  
             antiVaxIndex, data = d)

#### NEP ####
# Exploring the correlates with the NEP

# Demographic and Political variables
summary(lm(NEP ~  age + male + income + education + white + latino  + trump + region + biden, data = d))

# Kitchen Sink Model
summary(lm(NEP ~ likelihoodCovid + likelihoodFatalCovid + 
             worriedFamily + preexistCond + 
             positiveCovid + knowCovid + incomeLoss +
             maskFreqCommunity + moralObl + sra +
             paidSickLeave + insured +
             age + male + income + education + white + latino + trump + region + biden +
             lotterySwitchPoint +  
             vaccinesCovid + antiVaxIndex, data = d))

# Maximized Adj R2
nep <- lm(NEP ~ likelihoodFatalCovid + 
             worriedFamily + preexistCond +  
             knowCovid + incomeLoss +
             moralObl + 
             paidSickLeave + 
             age + male + white  + trump + region +
             lotterySwitchPoint +  
             antiVaxIndex, data = d)
summary(nep)


stargazer(rtb, rrb, rre,
          no.space = T,
          column.sep.width = "1pt",
          suppress.errors = T,
          font.size = "scriptsize",
          report = "vc*")
        