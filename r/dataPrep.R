# Data Prep for Analysis
# Created by: Trisha R. Shrum

load(file = 'data/workingDataset_dataQuality_fixZipState_merged.RData')

# Filtering valid responses for updated Cronbach's alpha scores
d %>% filter(invalidResponse == 0) -> d

#### Data Preparation ####

# Dropping Unneeded Variables
drops <- c("Status", "Progress", "Duration (in seconds)", "Finished", "RecordedDate",
           "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "DistributionChannel",
       "UserLanguage", "Q_RecaptchaScore", "Q91", "Q3", "Q4")
d <- d[ , !(names(d) %in% drops)]

# Pulling Demographic Variables for summary stats
# Awkward coding to avoid rewriting the summaryStats script after cleaning up d dataset
# d %>% dplyr::select(Q43, Q51, Q68, Q99, Q68, Q69, Q70, Q71, Q72, Q73, Q98) -> demogs

# Creating census divisions variable 
d$division <- NULL
d %>%
  filter(state %in% c("CA", "WA", "OR", "HI", "AK")) %>%
  mutate(division = "Pacific") -> dPacific
d %>%
  filter(state %in% c("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM")) %>%
  mutate(division = "Mountain") -> dMountain
d %>%
  filter(state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO")) %>%
  mutate(division = "WestNorthCentral") -> dWestNorthCentral
d %>%
  filter(state %in% c("WI", "MI", "IL", "IN", "OH")) %>%
  mutate(division = "EastNorthCentral") -> dEastNorthCentral
d %>%
  filter(state %in% c("TX", "OK", "AR", "LA")) %>%
  mutate(division = "WestSouthCentral") -> dWestSouthCentral
d %>%
  filter(state %in% c("KY", "TN", "MS", "AL")) %>%
  mutate(division = "EastSouthCentral") -> dEastSouthCentral
d %>%
  filter(state %in% c("WV", "MD", "DE", "DC", "VA", "NC", "SC", "GA", "FL")) %>%
  mutate(division = "SouthAtlantic") -> dSouthAtlantic
d %>%
  filter(state %in% c("NY", "PA", "NJ")) %>%
  mutate(division = "MiddleAtlantic") -> dMiddleAtlantic
d %>%
  filter(state %in% c("ME", "VT", "NH", "MA", "CT", "RI")) %>%
  mutate(division = "NewEngland")  -> dNewEngland
rbind(dPacific, dMountain, dWestNorthCentral, dEastNorthCentral, dWestSouthCentral, dEastSouthCentral,
      dSouthAtlantic, dMiddleAtlantic, dNewEngland) -> d1
d[!d$ResponseId %in% d1$ResponseId,] -> missing
d <- d1


#### Covid Risk Behavior ####

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

drops <- c("Q92", "Q94", "Q83_1", "Q83_2", "Q83_3", "Q7")
d <- d[ , !(names(d) %in% drops)]



##### Risk Exposure Index ####

# Covid Risk Perceptions

# Perceived safety of actions wrt Covid exposure
# Missing values replaced with average value to avoid dropping full index from skipped questions or "unsure" response
# See functions.R file for safe() function
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
x <- data.frame(d$groceryRisk, d$gather10Risk, d$gather50Risk, d$hospitalRisk, d$indoorDiningDistancingRisk,
                   d$indoorDiningNoDistancingRisk, d$outdoorDiningDistancingRisk, d$outdoorDiningNoDistancingRisk,
                   d$takeoutRisk, d$homeVisitRisk, d$packageRisk, d$takeoutRisk, d$homeVisitRisk, 
                   d$packageRisk, d$playgroundRisk, d$homeSurfacesRisk, d$householdRisk, d$indoorGymRisk,
                   d$outsideWalkRisk, d$greenSpaceRisk, d$beachRisk, d$campingRisk, d$stateParkRisk,
                   d$runningRisk, d$babysitterRisk, d$schoolRisk)
alpha(x, na.rm = T)  # Cronbach's alpha = 0.94
d$riskPerceptionBehaviorIndex <- scale(d$groceryRisk+ d$gather10Risk+ d$gather50Risk+ d$hospitalRisk+ d$indoorDiningDistancingRisk+
    d$indoorDiningNoDistancingRisk+ d$outdoorDiningDistancingRisk+ d$outdoorDiningNoDistancingRisk+
    d$takeoutRisk+ d$homeVisitRisk+ d$packageRisk+ d$takeoutRisk+ d$homeVisitRisk+ 
    d$packageRisk+ d$playgroundRisk+ d$homeSurfacesRisk+ d$householdRisk+ d$indoorGymRisk+
    d$outsideWalkRisk+ d$greenSpaceRisk+ d$beachRisk+ d$campingRisk+ d$stateParkRisk+
    d$runningRisk+ d$babysitterRisk+ d$schoolRisk)
sum(is.na(d$riskPerceptionBehaviorIndex))
x$riskPerceptionBehaviorIndex <- d$riskPerceptionBehaviorIndex
x[is.na(x$riskPerceptionBehaviorIndex),]  -> missingRiskPerceptionIndex
d <- d[,!grepl("^Q36_",names(d))]  # dropping original columns that have been converted

#Covid Risk Perception
d$likelihoodCovid <- as.numeric(d$Q37_1)
d$likelihoodFatalCovid <- as.numeric(d$Q38_1)
d$worriedCOVID <- scale(as.numeric(decode(d$Q41, search = c("Very worried", "Somewhat worried", "Not too worried", "Not worried at all"),
                          replace = c(4, 3, 2, 1))))
drops <- c("Q37_1", "Q38_1", "Q41")
d <- d[ , !(names(d) %in% drops)]

# Risk-Taking Behaviors taken in the last 7 days (yes/no = 1/0)
d %>%
  dplyr::select(matches("^Q5_")) -> s1
s1$na_count <- apply(s1, 1, function(x) sum(is.na(x)))

s <- data.frame(ResponseId = d$ResponseId, bars = yesNoUnsure(d$Q5_1), indoorDining = yesNoUnsure(d$Q5_2), 
                outdoorDining = yesNoUnsure(d$Q5_3), takeout = yesNoUnsure(d$Q5_4), 
                groceryStore = yesNoUnsure(d$Q5_5), 
                party10 = yesNoUnsure(d$Q5_8), party50 = yesNoUnsure(d$Q5_9), 
                indoorGym = yesNoUnsure(d$Q5_16), visitFriendsHouse = yesNoUnsure(d$Q5_6), 
                hostVisitors = yesNoUnsure(d$Q5_7), 
                contactNonHousehold = yesNoUnsure(d$Q5_14),
                outdoorGym = yesNoUnsure(d$Q5_17))
# Converting "unsure" to an average of: 1) the sample mean for each activity and 2) the individual mean for all activities (excluding unsures)
s$na_count <- apply(s, 1, function(x) sum(is.na(x)))
sum(s$na_count)
s[2:13] <- sapply(s[2:13],as.numeric)  # converting yes/no questions to numeric, all unsures are now NAs
s$party10 <- ifelse(s$party50 == 1, 1, s$party10)  # 40 people misunderstood this question... if they went to a party of 50 or more, then they, by definition, went to a party of 10 or more
s$socialExposureAvg <- rowMeans(s[,2:13], na.rm = T)  # calculating mean social exposure for each respondent, excluding NA's
m <- colMeans(s[2:13], na.rm = T)  # calculating means for each activity, excluding NAs
m <- c(1, m)
for (i in 2:13) {
  s[,i] <- ifelse(is.na(s[,i]), (m[i] + s$socialExposureAvg) / 2 , s[,i])
}
s %>%
  dplyr::select(-na_count, -socialExposureAvg) %>%
  full_join(d, by = "ResponseId") -> d


# Risk-Taking Behaviors Weighted by Perceived Risk
# Using Averages from the Data on Risk Perceptions of each of these activities to drive the index
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
#d$riskTakingBehaviorIndex <- d$barsW + d$indoorDiningW + d$outdoorDiningW + d$takeoutW + d$groceryW +
#  d$visitFriendsHouseW + d$hostVisitorsW + d$party10W + d$party50W + d$indoorGymW

# Comparing internal consistency of weighted vs unweighted risk taking behavior indices
#riskTakingBehaviorIndex_df <- as.data.frame(cbind(d$bars, d$indoorDining, d$outdoorDining, d$takeout, d$grocery,
#                                d$visitFriendsHouse, d$hostVisitors, d$party10, d$party50, d$indoorGym, d$contactNonHousehould))
#riskTakingBehaviorIndexW_df <- cbind(d$barsW, d$indoorDiningW, d$outdoorDiningW, d$takeoutW, d$groceryW,
#                                 d$visitFriendsHouseW, d$hostVisitorsW, d$party10W, d$party50W, d$indoorGymW)
#alpha(riskTakingBehaviorIndex_df, na.rm = T)  # alpha = 0.82
#alpha(riskTakingBehaviorIndexW_df, na.rm = T)  # alpha = 0.81
#rm(riskTakingBehaviorIndex_df, riskTakingBehaviorIndexW_df)


# Social distancing behaviors
socialEngagement_df <- cbind(d$socialPeople, d$socialFreq)
socialExposure_df <- cbind(d$socialPeople, d$socialFreq, d$barsW, d$visitFriendsHouseW, d$hostVisitorsW, 
                                 d$party10W, d$party50W, d$indoorDiningW, d$indoorGymW)
alpha(socialEngagement_df, na.rm = T)  # alpha = 0.76
alpha(socialExposure_df, na.rm = T)  # alpha = 0.84
rm(socialEngagement_df, socialExposure_df)
d$socialExposureIndex <- scale(scale(d$socialPeople) + scale(d$socialFreq) + d$barsW + d$visitFriendsHouseW +
  d$hostVisitorsW + d$party10W + d$party50W + d$indoorDiningW + d$indoorGymW)

socialRiskReduction_df <- cbind(d$socialMask, d$socialDistancing, d$socialOutdoors)
alpha(socialRiskReduction_df, na.rm = T) # alpha = 0.68
socialRiskReduction2_df <- data.frame(mask = d$socialMask, distance = d$socialDistancing)
alpha(socialRiskReduction2_df, na.rm = T) # alpha = 0.75
d$socialRiskReductionIndex <- scale(d$socialMask + d$socialDistancing)
rm(socialRiskReduction2_df, socialRiskReduction_df)



#### COVID Precaution Index ####
covidSafetyBehavior <- d[,grepl("^Q6_",names(d))]
apply(covidSafetyBehavior, c(1,2), yesNo) -> x
x <- data.frame(ResponseId = d$ResponseId, x)

# Other risk reduction activities from Q5 group of questions
x$quarantined <- yesNoUnsure(d$Q5_11)
x$stayedHome <- yesNoUnsure(d$Q5_12)
x$pod <- yesNoUnsure(d$Q5_18)
x[2:22] <- sapply(x[2:22],as.numeric)  # converting yes/no questions to numeric, all unsures are now NAs

alpha(x[2:22]) # Cronbach's Alpha = 0.87
x[is.na(x)] <- 0 # changing missing values to 0 to compute correlation matrix
x_cor <- cor(x[2:22])
round(x_cor, 2)

# scaling individual columns and summing the scaled valued and scaling the sum
x_scale <- apply(x[2:22], 2, scale)
x$riskReducingBehaviorIndex <- scale(rowSums(x_scale))

x %>%
  dplyr::select(ResponseId, riskReducingBehaviorIndex) %>%
  full_join(d, by = "ResponseId") -> d1

# Naming risk reduction activity variables for further analysis
d$washHands <- yesNo(d$Q6_1)
d$postponedWorkTravel <- yesNo(d$Q6_2)
d$postponedVacationTravel <- yesNo(d$Q6_3)
d$postponedWorkSchoolActivities <- yesNo(d$Q6_4)
d$postponedSocialActivities <- yesNo(d$Q6_5)
d$visitedDoctor <- yesNo(d$Q6_6)
d$postponedDoctor <- yesNo(d$Q6_7)
d$stockpiledFoodWater <- yesNo(d$Q6_8)
d$avoidedHighRiskPeople <- yesNo(d$Q6_9)
d$avoidedPublicSpaces <- yesNo(d$Q6_10)
d$prayed <- yesNo(d$Q6_11)
d$avoidedRestaurants <- yesNo(d$Q6_12)
d$stockpiledHandSanitizer <- yesNo(d$Q6_13)
d$workedHome <- yesNo(d$Q6_14)
d$wornMask <- yesNo(d$Q6_15)
d$stockpiledMeds <- yesNo(d$Q6_16)
d$socializedOutdoors <- yesNo(d$Q6_17)
d$avoidedSchoolDaycare <- yesNo(d$Q6_18)

#### Effectiveness of Risk Reduction Behaviors ####

# Perceived Effectiveness of Risk Reduction Behaviors
e <- data.frame(ResponseId = d$ResponseId)
e$maskEff <- effective(d$Q42_1)
e$prayEff <- effective(d$Q42_2)
e$washHandsEff <- effective(d$Q42_3)
e$seeDrSickEff <- effective(d$Q42_4)
e$seeDrExposedEff <- effective(d$Q42_5)
e$avoidPublicEff <- effective(d$Q42_6)
e$avoidHighRiskEff <- effective(d$Q42_7)
e$avoidHospitalsEff <- effective(d$Q42_8)
e$avoidRestaurantsEff <- effective(d$Q42_9)
e$avoidAirTravelEff <- effective(d$Q42_10)
e$avoidParksEff <- effective(d$Q42_11)
e$avoidIndoorSocialEff <- effective(d$Q42_12)
e$avoidOutdoorSocialEff <- effective(d$Q42_13)
e$closeSchoolsEff <- effective(d$Q42_14)
e$stay6ftEff <- effective(d$Q42_15)

# Checking and replacing NA values with mean of average efficacy per respondent and average efficacy per behavior
e$na_count <- apply(e, 1, function(x) sum(is.na(x)) )
d %>%
  dplyr::select(matches("^Q42_")) -> e1
e1$missing_count <- apply(e1, 1, function(x) sum(is.na(x)))
e$missing_count <- apply(e1, 1, function(x) sum(is.na(x)))  # only 2 rows have actual missing values-- each has one skipped question
e$avgEfficacy <- rowMeans(e[2:16], na.rm = T)
behaviorAvgEfficacy <- c(NA, colMeans(e[2:16], na.rm = T))
for (i in 2:16) {
  e[,i] <- ifelse(is.na(e[,i]), (behaviorAvgEfficacy[i] + e$avgEfficacy) / 2 , e[,i])
}

# scaling individual columns and summing the scaled valued and scaling the sum
e_scale <- apply(e[2:16], 2, scale)
e$riskReducingEfficacyIndex <- scale(rowSums(e_scale))
# replacing 3 observations with all "unsure" responses with the average value of 0 for the scale.
e$riskReducingEfficacyIndex <- ifelse(is.na(e$riskReducingEfficacyIndex), 0, e$riskReducingEfficacyIndex)

alpha(e[,2:16], na.rm = T, check.keys = TRUE)  # Cronbach's Alpha = 0.88

hist(e$riskReducingEfficacyIndex)
sum(is.na(e$riskReducingEfficacyIndex))

e %>%
  dplyr::select(-missing_count) %>%
  rename(unsureEfficacyCount = na_count) %>%
  left_join(d, by = "ResponseId") -> d

# Cleaning up variables
d <- d[,!grepl("^Q5_",names(d))]  # dropping original columns that have been converted
d <- d[,!grepl("^Q6_",names(d))]  # dropping original columns that have been converted
d <- d[,!grepl("^Q42_",names(d))]  # dropping original columns that have been converted


#### Covid Impact ####

# Covid Presumed Positive
d$positiveCovid <- ifelse(d$Q80 == "Yes, I tested positive" | d$Q80 == "Probably, I was told by a doctor that I likely had Covid-19 but did not test positive.", 1, 0) 
d$positiveCovid <- ifelse(!is.na(d$Q43_3), 1, d$positiveCovid)
d$positiveCovidFamily <- ifelse(!is.na(d$Q43_1), 1, 0)
table(d$positiveCovid)
d$presumptiveCovid <- ifelse(d$Q80 == "Probably, I was told by a doctor that I likely had Covid-19 but did not test positive.", 1, 0) 

d$knowCovid <- NA
d$knowCovid <- ifelse(d$Q43_5 == "No, I don't know anyone", 0, d$knowCovid)  # note the reverse coding from usual
d$knowCovid <- ifelse(!is.na(d$Q43_1) | !is.na(d$Q43_2) | !is.na(d$Q43_3) | !is.na(d$Q43_4), 1, d$knowCovid)

#d$knowICUFatal <- ifelse(d$Q46 == "No, I don't know anyone", 0, 1) ## the display of this question was miscoded. Do not use.
#d$knowPositiveCovid <- ifelse(d$Q48 == "No, I don't know anyone", 0, 1)  ## the display of this question was miscoded. Do not use.
d %>% dplyr::select(-Q80, -matches("^Q43_")) -> d

# Job/Income Loss
table(d$Q54_1_1)
table(d$Q54_1_2)
table(d$Q54_3)
table(d$Q54_4)

# Hierarchy of bad: reduced income at some point, reduced income now, furloughed/lost job at some point, furloughed/lost job now
d$lostIncome <- 0
d$lostIncome <- ifelse(!is.na(d$Q54_2_1), 1, d$lostIncome)
d$lostIncome <- ifelse(!is.na(d$Q54_2_2), 2, d$lostIncome)
table(d$lostIncome)

d$furloughed <- 0
d$furloughed <- ifelse(!is.na(d$Q54_3_1), 3, d$furloughed)
d$furloughed <- ifelse(!is.na(d$Q54_3_2), 4, d$furloughed)
table(d$furloughed)

d$lostJob <- 0
d$lostJob <- ifelse(!is.na(d$Q54_1_1), 5, d$lostJob)
d$lostJob <- ifelse(!is.na(d$Q54_1_2), 6, d$lostJob)
table(d$lostJob)

d$incomeLoss <- 0
d$incomeLoss <- ifelse(d$lostIncome != 0, d$lostIncome, d$incomeLoss)
d$incomeLoss <- ifelse(d$furloughed != 0, d$furloughed, d$incomeLoss)
d$incomeLoss <- ifelse(d$lostJob != 0, d$lostJob, d$incomeLoss)
table(d$incomeLoss)
d$incomeLoss <- scale(d$incomeLoss)
table(d$incomeLoss)

d <- d[,!grepl("^Q54_",names(d))]  # dropping original columns that have been converted

# Mental Health Questions
d$mentalNervous <- frequencyDays(d$Q55_1)/3
d$mentalWorrying <- frequencyDays(d$Q55_2)/3
d$mentalDepressed <- frequencyDays(d$Q55_3)/3
d$mentalApathy <- frequencyDays(d$Q55_4)/3
d$mentalLossControl <- oftenNever(d$Q56_1)/4
d$mentalProblems <- -1*oftenNever(d$Q56_2)/4 # Higher number is positive indicator of mental health
d$mentalGoingYourWay <- -1*oftenNever(d$Q56_3)/4 # Higher number is positive indicator of mental health\
d$mentalOverwhelmed <- oftenNever(d$Q56_4)/4
d <- d[,!grepl("^Q55_",names(d))]  # dropping original columns that have been converted
d <- d[,!grepl("^Q56_",names(d))]  # dropping original columns that have been converted

d$mentalHealthIndex <- scale(d$mentalNervous + d$mentalWorrying + d$mentalDepressed + d$mentalApathy +
  d$mentalLossControl + d$mentalProblems + d$mentalGoingYourWay + d$mentalOverwhelmed)
sum(is.na(d$mentalHealthIndex))
alpha(d[grepl("^mental",names(d))], na.rm = T)  # Cronbach's Alpha = 0.78

      

#### COVID Risk ####
# Looking at factors that increase COVID risks
# Elderly household member
# Not working from home? 
d$preexistCond <- ifelse(d$Q52 == "Yes", 1, 0)
d$paidSickLeave <- ifelse(d$Q51 == "0", 0, 1)
d$insured <- ifelse(d$Q49 == "Yes", 1, 0)
d$household65 <- ifelse(d$Q95_1 != "0", 1, 0)

d$age <- 2020 - as.numeric(d$Q99)
d$senior <- ifelse(d$age >= 65, 1, 0)
d$ageGroups <- NA
d$ageGroups <- ifelse(d$age > 19 & d$age < 30, "20s", d$ageGroups)
d$ageGroups <- ifelse(d$age > 29 & d$age < 40, "30s", d$ageGroups)
d$ageGroups <- ifelse(d$age > 39 & d$age < 50, "40s", d$ageGroups)
d$ageGroups <- ifelse(d$age > 49 & d$age < 60, "50s", d$ageGroups)
d$ageGroups <- ifelse(d$age > 59 & d$age < 70, "60s", d$ageGroups)
d$ageGroups <- ifelse(d$age > 69 & d$age < 80, "70s", d$ageGroups)

d %>% dplyr::select(-Q51, -Q52, -Q49) -> d
d$workFromHomeDays <- as.numeric(d$Q11)
d %>% dplyr::select(-Q11) -> d

# Individual's estimate of cases and deaths in their state over the past week
d$covidCaseEstimate <- as.numeric(d$Q42) 
d$covidFatalityEstimate <- as.numeric(d$Q79)
d %>% dplyr::select(-Q42, -Q79) -> d
d$pastWeekCasesEstimatePC <- d$covidCaseEstimate/d$pop
d$pastWeekDeathsEstimatePC <- d$covidFatalityEstimate/d$pop
d$caseEstimateError <- d$pastWeekCasesPC - d$pastWeekCasesEstimatePC 

d$popDensity <- d$popDensity/1000

#### Social Norms ####
# Community Social Distancing Measures
d$maskFreqCommunity <- as.numeric(d$Q8_1)
d$socialGatherFreqCommunity <- decode(d$Q9,
                                      search = c("Rare", "Less frequent than they were pre-COVID", 
                                                 "About as frequent as they were pre-COVID",
                                                 "More frequent than they were pre-COVID"),
                                      replace = c(0, 1, 2, 3))
d$socialGatherFreqCommunity <- scale(as.numeric(d$socialGatherFreqCommunity))
d %>% dplyr::select(-Q8_1, -Q9) -> d

# Moral obligation
d$moralObl <- scale(agreeScale(d$Q10))
table(d$moralObl)
sum(is.na(d$moralObl))
d %>% dplyr::select(-Q10) -> d



#### Altruism Scale ####
sra <- data.frame(ResponseId = d$ResponseId, sraNA(d$Q71_1), sraNA(d$Q71_2), sraNA(d$Q71_3), sraNA(d$Q71_4), sraNA(d$Q71_5),
                      sraNA(d$Q71_6), sraNA(d$Q71_7), sraNA(d$Q71_8), sraNA(d$Q71_9), sraNA(d$Q71_10),
                      sraNA(d$Q71_11), sraNA(d$Q71_12), sraNA(d$Q71_13), sraNA(d$Q71_14), sraNA(d$Q71_15),
                      sraNA(d$Q71_16), sraNA(d$Q71_17), sraNA(d$Q71_18), sraNA(d$Q71_19), sraNA(d$Q71_20))
alpha(sra[2:21], na.rm = T)

# replacing "not applicable" responses with avg of person mean and action mean
sra$personAvg <- rowMeans(sra[2:21], na.rm = T)
sraActAvg <- c(NA, colMeans(sra[2:21], na.rm = T))
for (i in 2:21) {
  sra[,i] <- ifelse(is.na(sra[,i]), (sraActAvg[i] + sra$personAvg) / 2 , sra[,i])
}
alpha(sra[2:21], na.rm = T) # Cronbach's alpha = 0.94
x <- apply(sra[2:21], 1, mean, na.rm = TRUE) 
d$sra <- scale(x)
sum(is.na(d$sra))

d <- d[, !grepl("^Q71_", names(d))]


#### Trust in Science ####
# Confidence for groups to act in the best interest of the public
# How much confidence, if any, do you have in each of the following to act in the best interests of the public? 
d$trustPoliticians <- scale(confidence(d$Q74_1))
d$trustMedia <- scale(confidence(d$Q74_2))
d$trustMilitary <- scale(confidence(d$Q74_3))
d$trustMedScientists <- scale(confidence(d$Q74_4))
d$trustScientists <- scale(confidence(d$Q74_5))
d$trustReligiousLeaders <- scale(confidence(d$Q74_6))
d$trustSchoolLeaders <- scale(confidence(d$Q74_7))
d$trustBusinessLeaders <- scale(confidence(d$Q74_8))
d$trustDoctors <- scale(confidence(d$Q74_9))
d <- d[, !grepl("^Q74_", names(d))]

trust_df <- data.frame(d$trustPoliticians, d$trustBusinessLeaders, d$trustDoctors, d$trustMedia,
                       d$trustMedScientists, d$trustMilitary, d$trustReligiousLeaders, d$trustSchoolLeaders,
                       d$trustScientists)
alpha(trust_df)  # alpha = 0.75
d$trust <- scale(d$trustPoliticians + d$trustBusinessLeaders + d$trustDoctors + d$trustMedia +
                d$trustMedScientists + d$trustMilitary + d$trustReligiousLeaders + d$trustSchoolLeaders +
                d$trustScientists)
sum(is.na(d$trust))
table(d$Q75)
sum(is.na(d$Q75))
d$scientistsStrongRoleInPolicy <- ifelse(d$Q75 == "Scientists should take an active role in public policy debates about scientific issues", 1, 0)
d$publicOpinionGuideSciencePolicy <- ifelse(d$Q77 == "Public opinion should play an important role to guide policy decisions about scientific
issues", 1, 0)
d$scienceExpertsBetterScientificIssues <- decode(d$Q78,
                                                 search = c("Usually BETTER at making good policy decisions about scientific issues than other
people", "Usually WORSE at making good policy decisions about scientific issues than other
people", "NEITHER BETTER NOR WORSE at making good policy decisions about scientific
issues than other people"),
                                                 replace = c("3", "1", "3"))
d %>% dplyr::select(-Q75, -Q77, -Q78) -> d

#### Vaccine Beliefs ####
d$vaccinesHarmful <- agreeScale(d$Q39_1)
d$vaccinesBenefits <- agreeScale(d$Q39_2)
d$vaccinesDeath <- agreeScale(d$Q39_3)
d$vaccinesEffective <- agreeScale(d$Q39_1)
d$vaccinesCovid <- scale(as.numeric(decode(d$Q40,
                          search = c("Very unlikely", "Somewhat unlikely", "Somewhat likely", "Very likely", "Unsure"),
                          replace = c("1", "2", "4", "5", "3"))))
d$antiVaxIndex <- scale(scale(d$vaccinesHarmful) - scale(d$vaccinesBenefits) + scale(d$vaccinesDeath) - scale(d$vaccinesEffective)) 
d <- d[, !grepl("^Q39_", names(d))]
d %>% dplyr::select(-Q40) -> d
sum(is.na(d$antiVaxIndex))

#### Holt & Laury Risk Coefficients ####
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
d %>% dplyr::select(-Q82, -Q83, -Q84, -Q85, -Q86, -Q87, -Q88, -Q89, -Q90) -> d

lot %>% 
  dplyr::select(ResponseId, lotterySwitchPoint, lotteryRational, lotteryMultiSwitch) %>%
  full_join(d, by = "ResponseId") -> d
sum(is.na(d$lotterySwitchPoint))
d$riskAversion <- scale(d$lotterySwitchPoint)

## New Environmental Paradigm Scale (Dunlap 2000) ##
# I am summing the NEP items and subtracting the Dominant Social Paradigm (DSP) items
d$NEP <- scale(agreeScale(d$Q68_1) + agreeScale(d$Q68_3) + agreeScale(d$Q68_5) + agreeScale(d$Q68_7) + 
  agreeScale(d$Q68_9) + agreeScale(d$Q68_11) + agreeScale(d$Q68_13) + agreeScale(d$Q68_15) +
  -1 * (agreeScale(d$Q68_2) + agreeScale(d$Q68_4) + agreeScale(d$Q68_6) + agreeScale(d$Q68_8) +
          agreeScale(d$Q68_10) + agreeScale(d$Q68_12) + agreeScale(d$Q68_14)))
NEP <- data.frame(agreeScale(d$Q68_1), agreeScale(d$Q68_3), agreeScale(d$Q68_5), agreeScale(d$Q68_7), 
                    agreeScale(d$Q68_9), agreeScale(d$Q68_11), agreeScale(d$Q68_13), agreeScale(d$Q68_15),
                    -1*agreeScale(d$Q68_2), -1*agreeScale(d$Q68_4), -1*agreeScale(d$Q68_6), -1*agreeScale(d$Q68_8),
                  -1*agreeScale(d$Q68_10), -1*agreeScale(d$Q68_12), -1*agreeScale(d$Q68_14))
alpha(NEP)
d <- d[,!grepl("^Q68_",names(d))]  # dropping original columns that have been converted

## Demographics ##
d$male <- ifelse(d$Q68 == "Male", 1, 0)
d$female <- ifelse(d$Q68 == "Female", 1, 0)
d$othergender <- ifelse(d$Q68 == "Non-binary" | d$Q68 == "Prefer to self-describe", 1, 0)
d$education <- scale(as.numeric(decode(d$Q71,
                                 search = c("Some high school (no diploma)", "High school graduate (incl. GED)",
                                            "Some college (no degree)", "Associates degree/technical school/apprenticeship", 
                                            "Bachelor’s degree", "Postgraduate (like Master’s, PhD) / professional degree (like JD)"),
                                 replace = c("1", "2", "3", "4", "5", "6"))))
d$bachelorsDegree <- ifelse(d$education >= 5, 1, 0)
d$income <- as.numeric(decode(d$Q72,
                              search = c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $24,999",
                                         "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999",
                                         "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999",
                                         "$200,000 or more"),
                              replace = c(10, 12.5, 20, 30, 42.5, 62.5, 87.5, 125, 175, 200)))
d$job <- d$Q96

d$white <- ifelse(!is.na(d$Q70_12), 1, 0)
d$nativeAmericanAlaskan <- ifelse(!is.na(d$Q70_1), 1, 0)
d$asianIndian <- ifelse(!is.na(d$Q70_2), 1, 0)
d$black <- ifelse(!is.na(d$Q70_3), 1, 0)
d$chamarro <- ifelse(!is.na(d$Q70_4), 1, 0)
d$chinese <- ifelse(!is.na(d$Q70_5), 1, 0)
d$filipino <- ifelse(!is.na(d$Q70_6), 1, 0)
d$japanese <- ifelse(!is.na(d$Q70_7), 1, 0)
d$korean <- ifelse(!is.na(d$Q70_8), 1, 0)
d$nativeHawaiian <- ifelse(!is.na(d$Q70_9), 1, 0)
d$samoan <- ifelse(!is.na(d$Q70_10), 1, 0)
d$vietnamese <- ifelse(!is.na(d$Q70_11), 1, 0)
d$otherRace <- ifelse(!is.na(d$Q70_12), 1, 0)
d$asian <- ifelse(d$asianIndian == 1 | d$chinese == 1 | d$japanese == 1 | d$korean == 1 | d$vietnamese == 1 | d$filipino == 1, 1, 0)
d$pacificIslander <- ifelse(d$chamarro == 1 | d$nativeHawaiian == 1 | d$samoan == 1, 1, 0)
d$latino <- ifelse(substr(d$Q69, 1, 1) == "Y", 1, 0)

d <- d[, !grepl("^Q70_", names(d))]

d$republican <- ifelse(d$Q73 == "Republican", 1, 0)
d$democrat <- ifelse(d$Q73 == "Democrat", 1, 0)
d$democraticSocialist <- ifelse(d$Q73 == "Democratic Socialist", 1, 0)
d$greenParty <- ifelse(d$Q73 == "Green Party", 1, 0)
d$independentUnaffiliated <- ifelse(d$Q73 == "Independent" | d$Q73 == "No affiliation", 1, 0)
d$libertarian <- ifelse(d$Q73 == "Libertarian", 1, 0)
d$progressive <- ifelse(d$Q73 == "Progressive", 1, 0)
d$teaParty <- ifelse(d$Q73 == "Tea Party", 1, 0)

d$trump <- ifelse(d$Q98 == "Donald Trump / Mike Pence (Republican)", 1, 0)
d$biden <- ifelse(d$Q98 == "Joe Biden / Kamala Harris (Democratic)", 1, 0)

d %>% dplyr::select(-Q99,-Q68, -Q69, -Q71, -Q72, -Q73, -Q98, -Q96) -> d


table(d$Q95_4)
sum(is.na(d$Q95_4))
d %>% dplyr::select(matches("^Q95_")) -> h
h <- as.data.frame(apply(h, 2, function(x) gsub("+", "", x, fixed = TRUE)))  # removing plus sign

d$householdSize <- as.numeric(h$Q95_1) + as.numeric(h$Q95_2) + as.numeric(h$Q95_3) + as.numeric(h$Q95_4)
table(d$householdSize)
sum(is.na(d$householdSize))
d$smallChildren <- as.numeric(h$Q95_4)
d$children <- as.numeric(h$Q95_4) + as.numeric(h$Q95_3)
d <- d[,!grepl("^Q95_",names(d))]
rm(h)

#### Final Cleanup of the Dataframe ####
rm(covidSafetyBehavior, lot, NEP)
rm(drops, lotteryMultiSwitch, lotteryNAs, lotterySwitchPoint, x)
rm(d1, dEastNorthCentral, dEastSouthCentral, dMiddleAtlantic, dMountain, dNewEngland, dPacific, dSouthAtlantic, dWestNorthCentral, dWestSouthCentral)
rm(e, e_scale, e1, missing, missingRiskPerceptionIndex, s, s1, trust_df, x_cor, x_scale, sra, l)

# Note: the "parent" variable was miscoded in Qualtrics and set equal to 1 for all participants. This does not affect the data.
d %>% dplyr::select(-`Q80_6_TEXT - Parent Topics`, -`Q80_6_TEXT - Topics`, -Q96_36_TEXT, -Q80_6_TEXT, 
                    -Q73_10_TEXT, -`Random ID`, -Q80_1, -parent) -> d


d <- d[, !grepl("^Q46_", names(d))]  # this question was improperly coded in the survey and did not show up for the relevant respondents
d <- d[, !grepl("^Q48_", names(d))]  # this question was improperly coded in the survey and did not show up for the relevant respondents

write.csv(d, "data/segsCovid_2020Survey_FullCleanDataSet.csv")

#### Outdoor Use Questions ####
# Moving all outdoor use questions to a separate dataframe for later analysis.
d %>%
  dplyr::select(ResponseId, Q12_1:Q12_8, Q13, Q14, Q15_1:Q15_10, Q16_1:Q16_8,
                Q32_1:Q32_12, Q34_1:Q34_12) -> outdoors
d %>%
  dplyr::select(-Q12_1:-Q12_8, -Q13, -Q14, -Q15_1:-Q15_10, -Q16_1:-Q16_8,
                -Q32_1:-Q32_12, -Q34_1:-Q34_12) -> d


write.csv(d, "data/segsCovid_2020Survey_CleanDataSet.csv")
write.csv(outdoors, "data/segsCovid_2020Survey_OutdoorsData.csv")

save(d, file = "data/prepped.R")


