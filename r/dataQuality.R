# Data Quality Script
# Written by Trisha R. Shrum
# Purpose: This script uses data quality checks to remove low-quality data from the primary analysis. 

d <- r 

#### Removing test responses ####
# First non-test participant began at 2020-10-07 10:32:12
# Prior responses were all survey tests
d <- d[which(d$StartDate == "2020-10-07 10:32:12"):nrow(d),]

# Removing test runs of the survey
d %>%
  filter(Q1 != "test-Incomplete" & Q1 != "test" & Q1 != "test-EC-newCompletionCode" & Q1 != "0aa0041f") -> d

# Removing anyone who did not consent to complete the study
d <- d[!is.na(d$Q91),]

# Removing participants who did not complete most of the survey
d$Progress <- as.numeric(d$Progress)
d %>% 
  filter(Progress < 50) -> incomplete
d <- d[d$Progress >= 50 & !is.na(d$Progress),]


#### Invalid Response Flag ####
# Codes: 0 = valid response, 1 = likely bot (identified by Qualtrics), 2 = likely repeat (identified by duplicate IP and fast response time)

d$invalidResponse <- 0  # Setting default as valid

# Flagging participants who are tagged as likely bots by Qualtrics fraud detection tools
# https://www.qualtrics.com/support/survey-platform/survey-module/survey-checker/fraud-detection/
d$invalidResponse <- ifelse(as.numeric(d$Q_RecaptchaScore) < 0.5, 1, d$invalidResponse)

# Flagging participants who completed the survey quickly (lowest quartile) & had a duplicate IP Address
d$completionTime <- ifelse(d$Progress >= 99, as.numeric(d$`Duration (in seconds)`)/60, NA)
quantile(d$completionTime, c(.01, .02, .025, .05, .1, .25, .5, .75, .9, .95), na.rm = TRUE)
summary(d$completionTime)
summary(lm(completionTime ~ duplicatedIPAddress, data = d))
lowestQuartile <- quantile(d$completionTime, c(.25), na.rm = TRUE)
d$invalidResponse <- ifelse(d$completionTime < lowestQuartile & d$duplicatedIPAddress == TRUE & d$invalidResponse == 0, 2, d$invalidResponse)

table(d$invalidResponse)

save(d, file ='data/workingDataset_dataQuality.RData')
rm(drop, incomplete)  

