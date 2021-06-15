# This script attempts to reconcile mismatched location data before merging with location based COVID case data
# Written by Trisha R. Shrum

# Loading working dataframe (d), last saved in dataQuality.R
load(file = 'data/workingDataset_dataQuality.RData')


# Location variables
# Q1: region variable - MTurk user was supposed to copy and paste this from the MTurk HIT, many used their MTurk ID instead
# Q97: self-reported zip code
# Q76: self-reported state of residence

# Naming geographic data collected from the survey
d$region <- toupper(substr(d$Q1, 1, 1))  # pulling out first initial from automated region code
d$regionCode_raw <- d$Q1  # saving automated region code
d$state <- d$Q76  # participant reported state
d$zip <- d$Q97  # participant reported zip code
d %>% dplyr::select(-Q1, -Q97, -Q76) -> d  # removing raw variables from d

d %>%
  dplyr::select(ResponseId, duplicatedIPAddress, invalidResponse, region, state, zip) -> l

# Creating state variable from zip code - 
l$stateFromZip <- NA
for (i in 1:nrow(l)) {
  if (!is.na(l$zip[i])) {
    l$stateFromZip[i] <- reverse_zipcode(l$zip[i])$state
  }
  if (is.na(l$zip[i])) {
    l$stateFromZip[i] <- NA
  }
}

# Creating state abbreviations variables from reported state
l$stateAbb <- state.abb[match(l$state,state.name)]  
l$stateAbb <- ifelse(l$state == "District of Columbia", "DC", l$stateAbb)
l[which(is.na(l$stateAbb)),]  # checking conversion

# Cross-check state and zip code
l$stateMatchZip <- ifelse(l$stateAbb == l$stateFromZip, 1, NA)  # if reported state matches reported zip, stateMatchZip == 1
l$stateMatchZip <- ifelse(!is.na(l$zip) & is.na(l$stateMatchZip), 0, l$stateMatchZip)  # if a zip code was reported, but does not match state, stateMatchZip == 0
table(l$stateMatchZip)  
sum(is.na(l$stateMatchZip))  # 3 NAs

# Creating validated states
l$stateValidated <- NA
l$stateValidated <- ifelse(l$stateMatchZip == 1, l$stateAbb, l$stateValidated)
l$stateValidated <- ifelse(l$stateMatchZip == 0, 0, l$stateValidated)
l$stateValidated <- ifelse(l$ResponseId == "R_2ZDKx0ssS2rxN4f", "SC", l$stateValidated)  # assuming typo on reported zip
l$stateValidated <- ifelse(l$ResponseId == "R_Aj04NAfPkgxyECt", "MO", l$stateValidated)  # assuming they chose montana instead of missouri, all other info is correct
l$stateValidated <- ifelse(l$ResponseId == "R_31abCtQlJqyRS8R", "CA", l$stateValidated)
l$stateValidated <- ifelse(l$ResponseId == "R_1IAbyzn7v5ufOcA", l$stateAbb, l$stateValidated)  # clear typo on reported zip

sum(is.na(l$stateValidated))
sum(is.na(l$state))
l %>%
  filter(is.na(stateValidated)) -> x


#### Creating validated zip codes ####
l$zipValidated <- NA

# If reported state and reported zip code do not match, considered invalid unless there is an indication of a valid mistake (typo,etc)
l$zipValidated <- ifelse(!is.na(l$stateValidated), l$zip, l$zipValidated)
l$zipValidated <- ifelse(l$ResponseId == "R_2ZDKx0ssS2rxN4f", "29621", l$zipValidated) # assuming a typo, close to IP address zip
l$zipValidated <- ifelse(l$ResponseId == "R_Aj04NAfPkgxyECt", l$zip, l$zipValidated) # chose incorrect state close in alphabetical order
l$zipValidated <- ifelse(l$ResponseId == "R_1IAbyzn7v5ufOcA", "06830", l$zipValidated) # assuming a typo, matches state
l$zipValidated <- ifelse(l$ResponseId == "R_31abCtQlJqyRS8R", "90012", l$zipValidated)  # assuming a typo, close to IP address zip and all other data seems valid
invalidZip <- c("41251", "95855", "97653")  # these zip codes do not exist in the US
l$zipValidated <- ifelse(l$zipValidated %in% invalidZip, NA, l$zipValidated)  # changing invalid zips to NA


sum(is.na(l$zipValidated))  # 7 invalid zips
l$zip <- l$zipValidated
sum(is.na(l$zip))  # 7 NA in zip

# Flagging mismatches as low quality data
l %>%
  dplyr::filter(stateValidated == 0) -> x

# Marking mismatched of reported zip code and reported state as invalid (after checking for typos)
l$invalidResponse <- ifelse(l$stateValidated == 0 & l$invalidResponse == 0, 3, l$invalidResponse)
table(l$invalidResponse)

# Changing state variable to reflect validated states unless zip code was missing and state was reported
l$state <- ifelse(!is.na(l$stateValidated), l$stateValidated, l$stateAbb)
l$state <- ifelse(l$stateValidated == "0", l$stateAbb, l$state)
table(l$state)
sum(is.na(l$state))  # 3 NA in state


#### Correcting Regions ####
l$region1 <- l$region
l$region1 <- ifelse(l$state == "AL", "S", l$region1)
l$region1 <- ifelse(l$state == "AZ", "W", l$region1)
l$region1 <- ifelse(l$state == "CA", "W", l$region1)
l$region1 <- ifelse(l$state == "CO", "W", l$region1)
l$region1 <- ifelse(l$state == "CT", "N", l$region1)
l$region1 <- ifelse(l$state == "FL", "S", l$region1)
l$region1 <- ifelse(l$state == "GA", "S", l$region1)
l$region1 <- ifelse(l$state == "ID", "W", l$region1)
l$region1 <- ifelse(l$state == "IL", "M", l$region1)
l$region1 <- ifelse(l$state == "IN", "M", l$region1)
l$region1 <- ifelse(l$state == "ME", "N", l$region1)
l$region1 <- ifelse(l$state == "MD", "S", l$region1)
l$region1 <- ifelse(l$state == "MA", "N", l$region1)
l$region1 <- ifelse(l$state == "MN", "M", l$region1)
l$region1 <- ifelse(l$state == "MO", "M", l$region1)
l$region1 <- ifelse(l$state == "MT", "W", l$region1)
l$region1 <- ifelse(l$state == "NV", "W", l$region1)
l$region1 <- ifelse(l$state == "NJ", "N", l$region1)
l$region1 <- ifelse(l$state == "NY", "N", l$region1)
l$region1 <- ifelse(l$state == "NC", "S", l$region1)
l$region1 <- ifelse(l$state == "OH", "M", l$region1)
l$region1 <- ifelse(l$state == "OK", "S", l$region1)
l$region1 <- ifelse(l$state == "PA", "N", l$region1)
l$region1 <- ifelse(l$state == "RI", "N", l$region1)
l$region1 <- ifelse(l$state == "TN", "S", l$region1)
l$region1 <- ifelse(l$state == "TX", "S", l$region1)
l$region1 <- ifelse(l$state == "UT", "W", l$region1)
l$region1 <- ifelse(l$state == "VA", "S", l$region1)
l$region1 <- ifelse(l$state == "WA", "W", l$region1)
l$region1 <- ifelse(l$region1 == "A", NA, l$region1)
table(l$region1, l$state)
l$region <- l$region1

#### Merging updated location data ####
l %>%
  dplyr::select(ResponseId, region, state, zip, invalidResponse) -> l

d %>%
  dplyr::select(-region, -state, -zip, -invalidResponse) %>%
  left_join(l, by = "ResponseId") -> d

save(d, file ='data/workingDataset_dataQuality_fixZipState.RData')
rm(l, x)

