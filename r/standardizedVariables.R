
load("data/prepped.R")

d$NEP <- scale(NEP)
d$sra <- scale(sra)
d$moralObl <- scale(d$moralObl)

d$education <- scale(d$education)

d$lotterySwitchPoint <- scale(d$lotterySwitchPoint)
d$mentalHealthIndex <- scale(d$mentalHealthIndex)

d$incomeLoss <- scale(d$incomeLoss)
d$socialGatherFreqCommunity <- scale(d$socialGatherFreqCommunity)

d$vaccinesCovid <- scale(d$vaccinesCovid)
d$antiVaxIndex <- scale(d$antiVaxIndex)
d$trustMedScientists <- scale(d$trustMedScientists)

d$worriedCOVID <- scale(d$worriedCOVID)

save(d, file = "data/prepped.R")

