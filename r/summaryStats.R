# Summary Statistics of Study Population
# Created by: Trisha R. Shrum

load("data/prepped.R")

d %>% filter(invalidResponse == 0) -> d

table(d$income)
(17+26+57)/nrow(d)
(56+130)/nrow(d)
169/nrow(d)
75/nrow(d)
(38+9+6)/nrow(d)

table(d$education)
2/nrow(d)
62/nrow(d)
63/nrow(d)

# Gender
table(d$male)
sum(d$male)/nrow(d)
table(d$female)
sum(d$female)/nrow(d)
sum(d$othergender)/nrow(d)

# Race/Ethnicity
sum(d$asian)/nrow(d)
sum(d$black)/nrow(d)
sum(d$latino)/nrow(d)
sum(d$nativeAmericanAlaskan)/nrow(d)
sum(d$pacificIslander)/nrow(d)
sum(d$white)/nrow(d)

# Political Affiliation
sum(d$democrat)/nrow(d)
sum(d$republican)/nrow(d)
sum(d$independentUnaffiliated)/nrow(d)
sum(d$democraticSocialist)/nrow(d)
sum(d$libertarian)/nrow(d)
sum(d$greenParty)/nrow(d)
sum(d$progressive)/nrow(d)
sum(d$teaParty)/nrow(d)
sum(d$libertarian)/nrow(d) + sum(d$greenParty)/nrow(d) + sum(d$progressive)/nrow(d) + sum(d$teaParty)/nrow(d)

table(d$ageGroups)
sum(d$ageGroups == "20s")/nrow(d)
sum(d$ageGroups == "30s")/nrow(d)
sum(d$ageGroups == "40s")/nrow(d)
sum(d$ageGroups == "50s")/nrow(d)
sum(d$ageGroups == "60s")/nrow(d)
sum(d$ageGroups == "70s")/nrow(d)

mean(d$age, na.rm = T)
mean(d$senior, na.rm = T)
mean(d$household65, na.rm = T)

table(d$preexistCond)
mean(d$preexistCond, na.rm = T)

table(d$positiveCovid)
mean(d$positiveCovid, na.rm = T)
mean(d$knowCovid, na.rm = T)

mean(d$trump, na.rm = T)
mean(d$biden, na.rm = T)


mean(d$insured, na.rm = T)
mean(d$paidSickLeave, na.rm = T)
