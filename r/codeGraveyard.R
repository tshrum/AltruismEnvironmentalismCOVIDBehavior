# Code Graveyard
# These are unlikely to be used and are moved here for clarity


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