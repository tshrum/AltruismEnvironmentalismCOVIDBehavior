# Data Quality Script
# Written by Trisha R. Shrum
# Purpose: This script uses data quality checks to remove low-quality data from the primary analysis. 

d <- r 

#### Removing test responses ####
# Small launch test launched on 10/07/2020
# Prior responses were all survey tests
d$StartDate <- as.Date(substr(d$StartDate, 1, 10), "%Y-%m-%d")
d <- d[d$StartDate >= "2020-10-07",]


# Removing test runs of the survey
d %>%
  filter(Q1 != "test-Incomplete" & Q1 != "test" & Q1 != "test-EC-newCompletionCode" & Q1 != "0aa0041f" & Q1 != "Scottt") -> d

# Removing anyone who did not consent to complete the study
d <- d[!is.na(d$Q91),]

# Removing participants who did not complete most of the survey
d$Progress <- as.numeric(d$Progress)
d %>% 
  filter(Progress < 75) -> incomplete
d <- d[d$Progress >= 75 & !is.na(d$Progress),]


#### Invalid Response Flag ####
# Codes: 0 = valid response, 1 = likely bot (identified by Qualtrics), 2 = likely repeat (identified by duplicate IP and fast response time)

d$invalidResponse <- 0  # Setting default as valid

# Flagging participants who are tagged as likely bots by Qualtrics fraud detection tools
# https://www.qualtrics.com/support/survey-platform/survey-module/survey-checker/fraud-detection/
d$invalidResponse <- ifelse(as.numeric(d$Q_RecaptchaScore) < 0.5, 1, d$invalidResponse)

# Flagging participants who completed the survey quickly (lowest quartile) & had a duplicate IP Address
d$completionTime <- ifelse(d$invalidResponse == 0, as.numeric(d$`Duration (in seconds)`)/60, NA)
quantile(d$completionTime, c(.01, .02, .025, .05, .1, .25, .5, .75, .9, .95), na.rm = TRUE)
summary(d$completionTime)
summary(lm(completionTime ~ duplicatedIPAddress, data = d))
lowestQuartile <- quantile(d$completionTime, c(.25), na.rm = TRUE)
d$invalidResponse <- ifelse(d$completionTime < lowestQuartile & d$duplicatedIPAddress == TRUE & d$invalidResponse != 1, 2, d$invalidResponse)
d$invalidResponse <- ifelse(d$completionTime < quantile(d$completionTime, c(.02), na.rm = TRUE) & d$invalidResponse != 1 & d$invalidResponse != 2, 3, d$invalidResponse)

table(d$invalidResponse)

save(d, file ='data/workingDataset_dataQuality.RData')
rm(incomplete)  

