# Preliminary Analysis of COVID survey launched by SEGS
# Created: October 29, 2020
# Created by: Trisha R. Shrum

library(tidyverse)
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
library(dplyr)
library(sjPlot)
library(ltm)
library(car)
library(psych)

source('~/Projects/SEGS_COVID/r/functions.R', echo=TRUE)

#### Data Preparation ####

d <- read_csv("data/SEGS_Qualtrics_Covid19-cleanIDs_2020-11-09.csv")

# Remove first two rows
d <- d[-1:-2,]

# Social Distancing Measures
d$socialPeople <- as.numeric(decode(d$Q92, 
                                    search = c("None", "1", "2-3", "4-6", "7-10", "10+"),
                                    replace = c("0", "1", "2.5", "5", "8.5", "10")))
d$socialFreq <- as.numeric(decode(d$Q94, 
                                  search = c("Never", "Once", "2-3 times", "Daily", "Multiple Times per Day"),
                                  replace = c("0", "1", "2.5", "7", "14")))

d$socialMask <- as.numeric(decode(d$Q83_1, 
                                  search = c("Never", "Sometimes", "About half the time", "Most of the time", "Always"), 
                                  replace = c("0", "1", "2", "3", "4")))
d$socialDistancing <- as.numeric(decode(d$Q83_2, 
                                  search = c("Never", "Sometimes", "About half the time", "Most of the time", "Always"), 
                                  replace = c("0", "1", "2", "3", "4")))
d$socialOutdoors <- as.numeric(decode(d$Q83_3, 
                                  search = c("Never", "Sometimes", "About half the time", "Most of the time", "Always"), 
                                  replace = c("0", "1", "2", "3", "4")))

d$compareToAvg <- d$Q7
table(d$Q7)

##### COVID Risky Behavior Index ####

# Using Averages from the Data on Risk Perceptions of each of these activities to drive the index
d$groceryRisk <- safe(d$Q36_1)
d$gather10Risk <- safe(d$Q36_2)
d$gather50Risk <- safe(d$Q36_3)
d$hospitalRisk <- safe(d$Q36_4)
d$indoorDiningNoDistancingRisk <- safe(d$Q36_5)
d$indoorDiningDistancingRisk <- safe(d$Q36_6)
d$outdoorDiningNoDistancingRisk <- safe(d$Q36_7)
d$outdoorDiningDistancingRisk <- safe(d$Q36_8)
d$takeoutRisk <- safe(d$Q36_9)
d$homeVisitRisk <- safe(d$Q36_10)
d$packageRisk <- safe(d$Q36_11)
d$playgroundRisk <- safe(d$Q36_12)
d$homeSurfacesRisk <- safe(d$Q36_13)
d$householdRisk <- safe(d$Q36_14)
d$indoorGymRisk <- safe(d$Q36_15)
d$outsideWalkRisk <- safe(d$Q36_16)
d$greenSpaceRisk <- safe(d$Q36_17)
d$beachRisk <- safe(d$Q36_18)
d$campingRisk <- safe(d$Q36_19)
d$stateParkRisk <- safe(d$Q36_20)
d$runningRisk <- safe(d$Q36_21)
d$babysitterRisk <- safe(d$Q36_22)
d$schoolRisk <- safe(d$Q36_23)


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

d$quarantined <- ifelse(d$Q5_11 == "Yes", 1, 0)
d$stayedHome <- ifelse(d$Q5_12 == "Yes", 1, 0)
d$outdoorExercise <- ifelse(d$Q5_15 == "Yes", 1, 0)

cbind(d, RiskBehaviors) -> d

# COVID Risk Reduction behavior Index
covidSafetyBehavior <- d[,53:70]  ## WARNING: CHANGES TO d dataframe WILL MESS UP THIS LINE OF CODE
apply(covidSafetyBehavior, c(1,2), yesNo) -> x
x <- as.data.frame(x)

d$washHandsW <- scale(x$Q6_1)
d$stockpileFoodW <- scale(x$Q6_8) 
d$stockpileMedsW <- scale(x$Q6_16) 
d$avoidHighRiskPeopleW <- scale(x$Q6_9) 
d$avoidCrowdsW <- scale(x$Q6_10) 
d$maskW <- scale(x$Q6_15)
d$mask <- x$Q6_15
d$outdoorSocialW <- scale(x$Q6_17)

y <- cbind(x$Q6_1, x$Q6_9, x$Q6_10, x$Q6_15, x$Q6_17)
alpha(x)
alpha(y)



d$riskReducingBehaviors <- d$washHandsW + d$stockpileFoodW + d$stockpileMedsW + d$avoidHighRiskPeopleW +
  d$avoidCrowdsW + d$maskW + d$outdoorSocialW
riskReducingBehaviors_df <- cbind(d$washHands, d$stockpileFood, d$stockpileMeds, d$avoidHighRiskPeople,
                                    d$avoidCrowds, d$mask, d$outdoorSocial)
riskReducingBehaviorsW_df <- cbind(d$washHandsW, d$stockpileFoodW, d$stockpileMedsW, d$avoidHighRiskPeopleW,
                                  d$avoidCrowdsW, d$maskW, d$outdoorSocialW)
alpha(riskReducingBehaviors_df, na.rm = T)

d$riskReducingBehaviors <- x$Q6_1 + x$Q6_2 + x$Q6_3 + x$Q6_4 + x$Q6_5 + x$Q6_6 + x$Q6_7 + x$Q6_8 + x$Q6_9 + 
  x$Q6_10 + x$Q6_11 + x$Q6_12 + x$Q6_13 + x$Q6_14 + x$Q6_15 + x$Q6_16 + x$Q6_17 + x$Q6_18

# Risk-Taking Behaviors Weighted by Perceived Risk
d$barsW <- scale(d$bars)*mean(d$indoorDiningDistancingRisk, na.rm = T)
d$indoorDiningW <- scale(d$indoorDining)*mean(d$indoorDiningDistancingRisk, na.rm = T)
d$outdoorDiningW <- scale(d$outdoorDining)*mean(d$outdoorDiningDistancingRisk, na.rm = T)
d$takeoutW <- scale(d$takeout) * mean(d$takeoutRisk, na.rm = T)
d$groceryW <- scale(d$groceryStore) * mean(d$groceryRisk, na.rm = T)
d$visitFriendsHouseW <- scale(d$visitFriendsHouse) * mean(d$homeVisitRisk, na.rm = T)
d$hostVisitorsW <- scale(d$hostVisitors) * mean(d$homeVisitRisk, na.rm = T)
d$party10W <- scale(d$party10) * mean(d$gather10Risk, na.rm = T)
d$party50W <- scale(d$party50) * mean(d$gather50Risk, na.rm = T)
d$indoorGymW <- scale(d$indoorGym) * mean(d$indoorGymRisk, na.rm = T)

d$riskTakingBehaviors <- d$barsW + d$indoorDiningW + d$outdoorDiningW + d$takeoutW + d$groceryW +
  d$visitFriendsHouseW + d$hostVisitorsW + d$party10W + d$party50W + d$indoorGymW
riskTakingBehaviors_df <- cbind(d$bars, d$indoorDining, d$outdoorDining, d$takeout, d$grocery,
                                  d$visitFriendsHouse, d$hostVisitors, d$party10, d$party50, d$indoorGym)
riskTakingBehaviorsW_df <- cbind(d$barsW, d$indoorDiningW, d$outdoorDiningW, d$takeoutW, d$groceryW,
                                d$visitFriendsHouseW, d$hostVisitorsW, d$party10W, d$party50W, d$indoorGymW)
alpha(riskTakingBehaviors_df, na.rm = T)
alpha(riskTakingBehaviorsW_df, na.rm = T)

riskBehaviors <- as.data.frame(cbind(riskTakingBehaviors_df, riskReducingBehaviors_df))
riskBehaviors$V11 <- scale(riskBehaviors$V11)
riskBehaviors$V12 <- scale(riskBehaviors$V12)
alpha(riskBehaviors, na.rm = T, check.keys = TRUE)

# Social distancing behaviors
d$socialRiskReduction <- (d$socialMask + d$socialDistancing + d$socialOutdoors) / 3
d$socialRisks <- d$socialPeople * (4 - d$socialRiskReduction) + d$socialFreq

socialRisks_df <- cbind(d$socialPeople, d$socialFreq)
alpha(socialRisks_df, na.rm = T)

d$covidRiskBehaviorIndex <- d$riskTakingBehaviors - d$riskReducingBehaviors + d$socialRisks
d$covidRiskBehaviorIndex2 <- d$riskTakingBehaviors - d$riskReducingBehaviors


#### COVID Risk ####
# Looking at factors that increase COVID risks

# Community Social Distancing Measures
d$maskFreqCommunity <- as.numeric(d$Q8_1)
table(d$maskFreqCommunity)


# Moral obligation
d$moralObl <- agreeScale(d$Q10)
table(d$moralObl, d$Q10)

## Altruism Scale
# People are abusing the "not applicable" option. I think people are saying N/A instead of saying they
# have never done it. I am going to treat it as a never answer. 
sra_df <- data.frame(sra(d$Q71_1), sra(d$Q71_2), sra(d$Q71_3), sra(d$Q71_4), sra(d$Q71_5), 
                     sra(d$Q71_6), sra(d$Q71_7), sra(d$Q71_8), sra(d$Q71_9), sra(d$Q71_10), 
                     sra(d$Q71_11), sra(d$Q71_12), sra(d$Q71_13), sra(d$Q71_14), sra(d$Q71_15), 
                     sra(d$Q71_16), sra(d$Q71_17), sra(d$Q71_18), sra(d$Q71_19), sra(d$Q71_20))
x <- apply(sra_df, 1, mean, na.rm = TRUE)
d$sra <- x

## Vaccine Beliefs
d$vaccinesHarmful <- agreeScale(d$Q39_1)
d$vaccinesBenefits <- agreeScale(d$Q39_2)
d$vaccinesDeath <- agreeScale(d$Q39_3)
d$vaccinesEffective <- agreeScale(d$Q39_1)
d$vaccinesCOVID <- decode(d$Q40,
                          search = c("Very unlikely", "Somewhat unlikely", "Somewhat likely", "Very likely", "Unsure"),
                          replace = c("1", "2", "3", "4", "5", "NA"))
d$antiVaxIndex <- d$vaccinesHarmful - d$vaccinesBenefits + d$vaccinesDeath - d$vaccinesEffective 


## Holt & Laury Risk Coefficients
# Creating dataframe of lottery coefficients
# lottery function (in functions.R file) converts lottery questions to numeric: 1 = Lottery A, 2 = Lottery B
lot <- data.frame(ResponseId = d$ResponseId, l1 = lottery(d$Q82), l2 = lottery(d$Q83), l3 = lottery(d$Q84), 
                  l4 = lottery(d$Q85), l5 = lottery(d$Q86), l6 = lottery(d$Q87), l7 = lottery(d$Q88), 
                  l8 = lottery(d$Q89), l9 = lottery(d$Q90))
# Setting NAs to 0 and removing rows with all NAs
# This is to make the functions work and these rows will be 
lot[is.na(lot)] <- 0
lot$sum <- rowSums(lot[, c(-1, -2)])
lot <- lot[lot$sum > 0,]
lot %>%
  dplyr::select(ResponseId) -> lot_ResponseId  # removing Response Id to separate dataframe for analysis
lot %>% 
  dplyr::select(-sum, -ResponseId) -> lot

lotterySwitchPoint <- apply(lot, 1, first_switch) #Applying first_switch function as defined in get_switch.R
lotteryNAs <- apply(lot, 1, count_na) #Applying count_na function as defined in get_switch.R
lotteryMultiSwitch <- apply(lot, 1, count_switches) #Applying count_switches function as defined in get_switch.R

lot <- cbind(lot_ResponseId, lot, lotterySwitchPoint, lotteryNAs, lotteryMultiSwitch)  # putting them back together
rm(lot_ResponseId)
lot$ResponseId <- as.character(lot$ResponseId)

lot$lotteryRational <- ifelse(lotteryMultiSwitch <= 1, 1, 0)

d %>% dplyr::select(-sum) -> d

lot %>% 
  dplyr::select(ResponseId, lotterySwitchPoint, lotteryRational, lotteryMultiSwitch) %>%
  full_join(d, by = "ResponseId") -> d


## New Environmental Paradigm Scale (Dunlap 2000) ##
# I am summing the NEP items and subtracting the Dominant Social Paradigm (DSP) items
d$NEP <- agreeScale(d$Q68_1) + agreeScale(d$Q68_3) + agreeScale(d$Q68_5) + agreeScale(d$Q68_7) + 
          agreeScale(d$Q68_9) + agreeScale(d$Q68_11) + agreeScale(d$Q68_13) + agreeScale(d$Q68_15) +
            -1 * (agreeScale(d$Q68_2) + agreeScale(d$Q68_4) + agreeScale(d$Q68_6) + agreeScale(d$Q68_8) +
                    agreeScale(d$Q68_10) + agreeScale(d$Q68_12) + agreeScale(d$Q68_14))


## Demographics ##
d$age <- 2020 - as.numeric(d$Q99)
d$male <- ifelse(d$Q68 == "Male", 1, 0)
d$education <- as.numeric(decode(d$Q71,
                                 search = c("Some high school (no diploma)", "High school graduate (incl. GED)",
                                 "Some college (no degree)", "Associates degree/technical school/apprenticeship", 
                                 "Bachelor’s degree", "Postgraduate (like Master’s, PhD) / professional degree (like JD)"),
                                 replace = c("1", "2", "3", "4", "5", "6")))
d$income <- as.numeric(decode(d$Q72,
                              search = c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999",
                                         "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
                                         "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
                                         "$200,000 or more"),
                              replace = c(10, 12.5, 20, 30, 42.5, 62.5, 87.5, 125, 175, 200)))
d$white <- ifelse(d$Q70 == "White", 1, 0)
d$latino <- ifelse(substr(d$Q69, 1, 1) == "Y", 1, 0)

d$republican <- ifelse(d$Q73 == "Republican", 1, 0)
d$democrat <- ifelse(d$Q73 == "Democrat", 1, 0)
d$trump <- ifelse(d$Q98 == "Donald Trump / Mike Pence (Republican)", 1, 0)
d$biden <- ifelse(d$Q98 == "Joe Biden / Kamala Harris (Democratic)", 1, 0)

d$region <- toupper(substr(d$Q1, 1, 1))

# Correcting Regions
d$region1 <- ifelse(d$Q76 == "Arizona", "W", d$region)
d$region1 <- ifelse(d$Q76 == "California", "W", d$region1)
d$region1 <- ifelse(d$Q76 == "Colorado", "W", d$region1)
d$region1 <- ifelse(d$Q76 == "Connecticut", "N", d$region1)
d$region1 <- ifelse(d$Q76 == "Florida", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Georgia", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Idaho", "W", d$region1)
d$region1 <- ifelse(d$Q76 == "Illinoi", "M", d$region1)
d$region1 <- ifelse(d$Q76 == "Maine", "N", d$region1)
d$region1 <- ifelse(d$Q76 == "Maryland", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Massachusetts", "N", d$region1)
d$region1 <- ifelse(d$Q76 == "Minnesota", "M", d$region1)
d$region1 <- ifelse(d$Q76 == "Montana", "W", d$region1)
d$region1 <- ifelse(d$Q76 == "New Jersey", "N", d$region1)
d$region1 <- ifelse(d$Q76 == "New York", "N", d$region1)
d$region1 <- ifelse(d$Q76 == "North Carolina", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Ohio", "M", d$region1)
d$region1 <- ifelse(d$Q76 == "Texas", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Virginia", "S", d$region1)
d$region1 <- ifelse(d$Q76 == "Washington", "W", d$region1)

d$region <- d$region1

#### Analysis ####

# Ordered logistic regression of frequency of personal mask wearing on the frequency of community mask wearing
m <- polr(as.factor(socialMask) ~ scale(maskFreqCommunity) + region + age + male + income + education + white + latino + republican, data = d, Hess = TRUE)
summary(m)
(ctable <- coef(summary(m))) ## store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table

# Ordered logistic regression of frequency of personal mask wearing on the frequency of community mask wearing, plus other variables
m <- polr(as.factor(socialMask) ~ scale(maskFreqCommunity) + moralObl + region + age + male + income + education + white + latino + republican, data = d, Hess = TRUE)
summary(m)
(ctable <- coef(summary(m))) ## store table
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 ## calculate and store p values
(ctable <- cbind(ctable, "p value" = p)) ## combined table

summary(lm(socialPeople ~ moralObl, data = d))
summary(lm(socialFreq ~ moralObl, data = d))
summary(lm(socialOutdoors ~ moralObl, data = d))
summary(lm(socialDistancing ~ moralObl, data = d))

summary(lm(scale(socialPeople) ~ scale(sra), data = d))
summary(lm(scale(socialFreq) ~ scale(sra), data = d))
summary(lm(scale(socialOutdoors) ~ scale(sra), data = d))
summary(lm(scale(socialMask) ~ scale(sra), data = d))

summary(lm(scale(socialPeople) ~ scale(antiVaxIndex), data = d))
summary(lm(scale(socialFreq) ~ scale(antiVaxIndex), data = d))
summary(lm(scale(socialOutdoors) ~ scale(antiVaxIndex), data = d))
summary(lm(scale(socialDistancing) ~ scale(antiVaxIndex), data = d))
summary(lm(scale(socialMask) ~ scale(antiVaxIndex), data = d))


summary(lm(scale(socialPeople) ~ scale(lotterySwitchPoint), data = d, subset = (d$lotteryRational == 1)))
summary(lm(scale(socialFreq) ~ scale(lotterySwitchPoint), data = d, subset = (d$lotteryRational == 1)))
summary(lm(scale(socialOutdoors) ~ scale(lotterySwitchPoint), data = d, subset = (d$lotteryRational == 1)))
summary(lm(scale(socialDistancing) ~ scale(lotterySwitchPoint), data = d, subset = (d$lotteryRational == 1)))
summary(lm(scale(socialMask) ~ scale(lotterySwitchPoint), data = d, subset = (d$lotteryRational == 1)))
summary(lm(scale(socialPeople) ~ scale(lotterySwitchPoint), data = d))
summary(lm(scale(socialFreq) ~ scale(lotterySwitchPoint), data = d))
summary(lm(scale(socialOutdoors) ~ scale(lotterySwitchPoint), data = d))
summary(lm(scale(socialDistancing) ~ scale(lotterySwitchPoint), data = d))
summary(lm(scale(socialMask) ~ scale(lotterySwitchPoint), data = d))

summary(lm(scale(socialPeople) ~ scale(NEP), data = d ))
summary(lm(scale(socialFreq) ~ scale(NEP), data = d))
summary(lm(scale(socialOutdoors) ~ scale(NEP), data = d))
summary(lm(scale(socialDistancing) ~ scale(NEP), data = d))
summary(lm(scale(socialMask) ~ scale(NEP), data = d))

summary(lm(scale(socialPeople) ~ scale(NEP) + scale(sra) + scale(lotterySwitchPoint), data = d ))


# Kitchen Sink Model
summary(lm(scale(socialPeople) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + Trump, data = d))
summary(lm(scale(socialFreq) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + Trump, data = d))
summary(lm(scale(socialOutdoors) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + Trump, data = d))
summary(lm(scale(socialDistancing) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + Trump, data = d))
summary(lm(scale(socialMask) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + Trump, data = d))

summary(lm(scale(covidRiskBehaviorIndex) ~ maskFreqCommunity + moralObl + scale(sra) + scale(antiVaxIndex)  +
             scale(NEP) + age + male + income + education + trump, data = d))

summary(lm(scale(covidRiskBehaviorIndex) ~ scale(sra), data = d))
summary(lm(scale(covidRiskBehaviorIndex) ~ scale(sra), data = d))

summary(lm(scale(socialPeople) ~ maskFreqCommunity + moralObl + age + male + income + education + Trump, data = d))


summary(lm(scale(sra) ~ age + male + income + education + Trump, data = d))
summary(lm(scale(sra) ~ Trump + Biden + age + male + income + education, data = d))
summary(lm(scale(sra) ~ Republican + Democrat + age + male + income + education, data = d))
linearHypothesis(lm(scale(sra) ~ Trump + Biden, data = d), "Trump = Biden")
summary(lm(scale(NEP) ~ trump + biden + age + male + income + education, data = d))




# Covid Risk Behavior Models
summary(lm(scale(covidRiskBehaviorIndex) ~ scale(moralObl) + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint+ age + male + income + education + region + white + latino + republican, data = d))
summary(lm(riskTakingBehaviors ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican + democrat, data = d))
summary(lm(riskReducingBehaviors ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican, data = d))
summary(lm(socialRisks ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican, data = d))
summary(lm(socialRiskReduction ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican, data = d))
summary(lm(socialPeople ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican, data = d))
summary(lm(socialFreq ~ moralObl + scale(sra) + scale(NEP) + scale(antiVaxIndex) + maskFreqCommunity + lotterySwitchPoint + age + male + income + education + region + white + latino + republican, data = d))


summary(lm(covidRiskBehaviorIndex ~ region, data = d))
summary(lm(covidRiskBehaviorIndex2 ~ region, data = d))
summary(lm(riskTakingBehaviors ~ region, data = d))
summary(lm(riskTakingBehaviors ~ Q76, data = d))

summary(lm(riskReducingBehaviors ~ region1, data = d))
summary(lm(socialRisks ~ region1, data = d))
summary(lm(moralObl ~ region1, data = d))
summary(lm(moralObl ~ region1, data = d))
summary(lm(maskFreqCommunity ~ d$Q76, data = d))
summary(lm(maskFreqCommunity ~ region1, data = d))
summary(lm(socialPeople ~ Q76, data = d))

table(d$region, d$Q76)

