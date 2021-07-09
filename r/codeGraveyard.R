# Code Graveyard
# These are unlikely to be used and are moved here for clarity

#### Back Parking Lot ####

# pulling 
d %>%
  dplyr::select(socialExposureIndex, NEP,
                age, male, income, education, white, latino, trump, biden, riskAversion,  
                mentalHealthIndex, 
                preexistCond, household65, smallChildren, likelihoodFatalCovid, likelihoodCovid,
                positiveCovid, knowCovid, incomeLoss, 
                maskFreqCommunity, socialGatherFreqCommunity, 
                popDensity, pastWeekCasesPC,
                paidSickLeave, insured,
                vaccinesCovid, antiVaxIndex, trustMedScientists, worriedCOVID) -> d1
d1[complete.cases(d1),] -> d2

summary(lm(sraNAremoved ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             riskAversion + trump + biden, data = d))
summary(lm(sraNAomittedInMean ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             riskAversion + trump + biden, data = d))
summary(lm(sra ~ NEP + division + popDensity +  age + male + income + education + white + latino + 
             riskAversion + trump + biden, data = d))



summary(lm(NEP ~ worriedCOVID + moralObl*sra, data = d))
m3 <- lm(NEP ~ sra + moralObl + worriedCOVID + age + male + income + education + white + latino + trump + biden +
           riskAversion, data = d)
m4 <- lm(socialExposureIndex ~ NEP +  
           age + male + income + education + white + latino + trump + biden + popDensity + 
           likelihoodCovid + preexistCond + household65 + smallChildren +
           positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
           maskFreqCommunity + socialGatherFreqCommunity + 
           popDensity + pastWeekCasesPC +
           paidSickLeave + insured +
           riskAversion +  
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
           riskAversion +  
           vaccinesCovid + antiVaxIndex + trustMedScientists +
           worriedCOVID + moralObl + sra, data = d)



## Social Exposure Index ~ NEP + Moral Obligation ##
m1 <- lm(socialExposureIndex ~ NEP + moralObl, data = d)
summary(m1)
m2 <- lm(socialExposureIndex ~ NEP + moralObl + 
           age + male + income + education + white + latino + trump + biden + riskAversion +
           mentalHealthIndex, 
         data = d)
m3 <- lm(socialExposureIndex ~ NEP + moralObl +
           age + male + income + education + white + latino + trump + biden + riskAversion +  
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
             riskAversion +  
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
             riskAversion +  
             vaccinesCovid + antiVaxIndex + trustMedScientists +
             worriedCOVID + moralObl + sra, data = d))

summary(lm(socialExposureIndex ~ 
             age + male + income + education + white + latino + trump + biden + popDensity + 
             likelihoodCovid + preexistCond + household65 + smallChildren +
             positiveCovid + knowCovid + incomeLoss + mentalHealthIndex +
             maskFreqCommunity + socialGatherFreqCommunity + 
             popDensity + pastWeekCasesPC +
             paidSickLeave + insured +
             riskAversion +  
             vaccinesCovid + antiVaxIndex + trustMedScientists +
             worriedCOVID + moralObl + sra + tightness, data = d))

summary(lm(pastWeekCasesPC ~ tightness + trumpPercent, data = d))
summary(lm(pastWeekCasesPC ~ tightness, data = d))
summary(lm(socialExposureIndex ~ tightness + trump, data = d))
summary(lm(socialExposureIndex ~ tightness + pastWeekCasesPC, data = d))
summary(lm(sra ~ tightness + age + male + income + education + white + latino + trump + biden + popDensity, data = d))


# IP Addressed based location validation
# Does state info from IP and reported match?
l$stateMatchIP <- ifelse(l$state == l$state_IPAddress, 1, 0)
table(l$stateMatchIP)
sum(is.na(l$stateMatchIP))

# If reported state and reported zip code match AND they match the IP state, then use the reported zip code
l$zipValidated <- ifelse(l$stateMatchZip == 1 & l$stateMatchIP == 1 & !is.na(l$stateMatchZip), l$zip, l$zipValidated)

l$zipValidated <- ifelse(l$stateMatchIP == 0 & l$region == "A" | is.na(l$stateMatchIP) & l$region == "A", "invalid", l$zipValidated)

## NEXT STEPS: Decide what to do with mismatched reported location and IP location
# ideas -- if they mismatch but the participant correctly provided the Region code (not MTurk ID) and the region and state match, then keep them
# possibly allow those who incorrectly provide Region code but who match



sum(is.na(l$stateValidated))
l[which(is.na(l$state)),]
for (i in 1:nrow(l)) { # filling in missing states from zip codes
  if(is.na(l$state[i]) & !is.na(l$zip[i])) {
    l$state[i] <- reverse_zipcode(l$zip[i])$state
  }
}

t$state <- state.abb[match(t$State,state.name)]  # converting to state abbreviations

# High Risk Behaviors: bars, indoor dining, 10+ gathering, 50+ gathering, indoor yoga/fitness
highRiskBehaviors <- data.frame(bars = yesNo(d$Q5_1), indoorDining = yesNo(d$Q5_2), 
                                party10 = yesNo(d$Q5_8), party50 = yesNo(d$Q5_9), 
                                indoorGym = yesNo(d$Q5_16)) 

# Medium Risk Behaviors: friends/relatives house, had visitors, non-household close contact,
#                        outdoor yoga/fitness
mediumRiskBehaviors <- data.frame(visitFriendsHouse = yesNo(d$Q5_6), hostVisitors = yesNo(d$Q5_7), 
                                  contactNonHousehould = yesNo(d$Q5_14),
                                  outdoorGym = yesNo(d$Q5_17))

# Low Risk Behaviors: outdoor dining, takeout, grocery store/pharmacy, hospital/health care facility,
#                     stayed at home except for essentials, household close contact, pod
lowRiskBehaviors <- data.frame(outdoorDining = yesNo(d$Q5_3), takeout = yesNo(d$Q5_4), 
                               groceryStore = yesNo(d$Q5_5),
                               healthCare = yesNo(d$Q5_10), contactHousehold = yesNo(d$Q5_13), 
                               pod = yesNo(d$Q5_18))

highRiskBehaviors$sum <- apply(highRiskBehaviors, 1, sum) 
mediumRiskBehaviors$sum <- apply(mediumRiskBehaviors, 1, sum) 
lowRiskBehaviors$sum <- apply(lowRiskBehaviors, 1, sum) 

RiskBehaviors <- cbind(highRiskBehaviors, mediumRiskBehaviors, lowRiskBehaviors)
cronbach.alpha(RiskBehaviors, na.rm = TRUE)

#d$covidRiskBehaviorIndex <- highRiskBehaviors$sum * 3 + mediumRiskBehaviors$sum * 2 + lowRiskBehaviors$sum * 1
#d$highRiskBehaviors <- highRiskBehaviors$sum


# Risk Reducing Behaviors ####
# Attempted to create a subset of behaviors, but cronbach's alpha was unacceptably low. Sticking with the full list.

d$washHandsW <- scale(x$Q6_1)
d$stockpileFoodW <- scale(x$Q6_8) 
d$stockpileMedsW <- scale(x$Q6_16) 
d$avoidHighRiskPeopleW <- scale(x$Q6_9) 
d$avoidCrowdsW <- scale(x$Q6_10) 
d$maskW <- scale(x$Q6_15)
d$mask <- x$Q6_15
d$outdoorSocialW <- scale(x$Q6_17)

y <- cbind(x$Q6_1, x$Q6_9, x$Q6_10, x$Q6_15, x$Q6_17)


d$riskReducingBehaviors <- d$washHandsW + d$stockpileFoodW + d$stockpileMedsW + d$avoidHighRiskPeopleW +
  d$avoidCrowdsW + d$maskW + d$outdoorSocialW
riskReducingBehaviors_df <- cbind(d$washHands, d$stockpileFood, d$stockpileMeds, d$avoidHighRiskPeople,
                                  d$avoidCrowds, d$mask, d$outdoorSocial)
riskReducingBehaviorsW_df <- cbind(d$washHandsW, d$stockpileFoodW, d$stockpileMedsW, d$avoidHighRiskPeopleW,
                                   d$avoidCrowdsW, d$maskW, d$outdoorSocialW)
alpha(riskReducingBehaviors_df, na.rm = T)


vse_maxR2 <- lm(socialExposureIndex ~ NEP + moralObl + sra +
                  age + income  + latino + trump + 
                  smallChildren + likelihoodFatalCovid + 
                  knowCovid + scale(incomeLoss) + 
                  maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                  popDensity + pastWeekCasesPC +
                  insured +
                  scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_maxR2)
vse_maxR2_noNEP <- lm(socialExposureIndex ~  moralObl +  sra +
                        age + income  + latino + trump + 
                        smallChildren + likelihoodFatalCovid + 
                        knowCovid + scale(incomeLoss) + 
                        maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                        popDensity + pastWeekCasesPC + 
                        insured + 
                        scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_maxR2_noNEP)
vse_maxR2_noMO <- lm(socialExposureIndex ~  NEP +  sra +
                       age + income  + latino + trump + 
                       smallChildren + likelihoodFatalCovid + 
                       knowCovid + scale(incomeLoss) + 
                       maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                       popDensity + pastWeekCasesPC + 
                       insured + 
                       scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_maxR2_noMO)
vse_maxR2_noNEPnoMO <- lm(socialExposureIndex ~  sra +
                            age + income  + latino + trump + 
                            smallChildren + likelihoodFatalCovid + 
                            knowCovid + scale(incomeLoss) + 
                            maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                            popDensity + pastWeekCasesPC + 
                            insured + 
                            scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_maxR2_noNEPnoMO)
vse_maxR2_noNEPnoMOnoSRA <- lm(socialExposureIndex ~  
                                 age + income  + latino + trump + 
                                 smallChildren + likelihoodFatalCovid + 
                                 knowCovid + scale(incomeLoss) + 
                                 maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                                 popDensity + pastWeekCasesPC + 
                                 insured + 
                                 scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_maxR2_noNEPnoMOnoSRA)
vse_maxR2_noVAX <- lm(socialExposureIndex ~  NEP + moralObl + sra +
                        age + income  + latino + trump +  
                        smallChildren + likelihoodFatalCovid + 
                        knowCovid + scale(incomeLoss) + 
                        maskFreqCommunity + scale(socialGatherFreqCommunity) + 
                        popDensity + pastWeekCasesPC + 
                        insured, data = d)
summary(vse_maxR2_noVAX)
vse_noSocialNorms <- lm(socialExposureIndex ~ NEP + moralObl + sra +
                          age + income  + latino + trump + 
                          smallChildren + likelihoodFatalCovid + 
                          knowCovid + scale(incomeLoss) + 
                          popDensity + pastWeekCasesPC +
                          insured +
                          scale(vaccinesCovid) + scale(antiVaxIndex), data = d)
summary(vse_noSocialNorms)

vse_noNEP_MO_VAX_SocialNorms <- lm(socialExposureIndex ~ 
                                     age + income  + latino + trump + 
                                     smallChildren + likelihoodFatalCovid + 
                                     knowCovid + scale(incomeLoss) + 
                                     popDensity + pastWeekCasesPC +
                                     insured, data = d)
summary(vse_noNEP_MO_VAX_SocialNorms)

summary(vse_all1)
summary(vse_all2)
summary(vse_all3)
summary(vse_all4)
summary(vse_all5)