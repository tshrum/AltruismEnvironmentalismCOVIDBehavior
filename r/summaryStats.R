# Summary Statistics of Study Population
# Created by: Trisha R. Shrum

table(d$age)
mean(d$age, na.rm = T)
mean(d$senior, na.rm = T)

mean(d$household65, na.rm = T)

table(d$preexistCond)
mean(d$preexistCond, na.rm = T)
mean(d$insured, na.rm = T)
mean(d$paidSickLeave)
table(demogs$Q51)

table(demogs$Q43)

table(d$education)
table(demogs$Q71)
(339 + 62)/581
2/581
65/581
(49 + 64)/581
339/581
62/581

table(demogs$Q68)
352/581
227/581
1/581

table(demogs$Q70)
(9 + 3 + 1 + 1 + 1 + 6)/581  # Native American
(3 + 1 + 1 + 1 + 57 + 1 + 2)/581 # Black
(3 + 1 + 1 + 1 + 1 + 6 + 2 + 1 + 1 + 465 + 1)/581 # White
(3 + 1 + 1 + 1 + 9 + 1 + 1 + 8 + 1 + 1 + 1 + 1 + 1)/581 # Asian
(3 + 1 + 1 + 1 + 6 + 1 + 1 + 2 +1 + 1)/581

table(demogs$Q69)
1 - 496/581

table(demogs$Q73)
224/581
199/581
(68 + 23)/581
44/581
7/581
(5 + 2 + 6 + 3)/581

table(demogs$Q98)
251/581
279/581
17/581
19/581

table(demogs$Q72)
(18 + 27 + 61)/581  # 25K or less
(55 + 121)/581  # 25K-50K
(174)/581  # 50K-75K
76/581    # 75K-100K
(34 + 9 + 6)/581  # 100k or more