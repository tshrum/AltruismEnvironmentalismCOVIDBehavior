# VAERS Vaccine Data

x <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2021VAERSData/2021VAERSDATA.csv")
y <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2021VAERSData/2021VAERSVAX.csv")
z <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2021VAERSData/2021VAERSSYMPTOMS.csv")

full_join(x, y) %>%
  filter(VAX_TYPE == "COVID19") -> c

c %>% 
  filter(DIED == "Y") -> deaths2021

x <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2020VAERSData/2020VAERSDATA.csv")
y <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2020VAERSData/2020VAERSVAX.csv")
z <- read_csv("/Users/tshrum/OneDrive - University of Vermont/Research/COVID/2020VAERSData/2020VAERSSYMPTOMS.csv")

full_join(x, y) %>%
  filter(VAX_TYPE == "COVID19") -> c

c %>% 
  filter(DIED == "Y") -> deaths2020

deaths <- rbind(deaths2020, deaths2021)


deaths$suicide <- 0
deaths$suicide <- ifelse(grepl("suicide", deaths$SYMPTOM_TEXT, ignore.case = T), 1, deaths$suicide)
table(deaths$suicide)
deaths$hospice <- 0
deaths$hospice <- ifelse(grepl("hospice", deaths$SYMPTOM_TEXT, ignore.case = T) | grepl("hospice", deaths$CUR_ILL, ignore.case = T) | grepl("hospice", deaths$HISTORY, ignore.case = T), 1, deaths$hospice)
deaths$hospice <- ifelse(grepl("palliative", deaths$SYMPTOM_TEXT, ignore.case = T) | grepl("palliative", deaths$CUR_ILL, ignore.case = T) | grepl("palliative", deaths$HISTORY, ignore.case = T), 1, deaths$hospice)
deaths$hospice <- ifelse(grepl("failure to thrive", deaths$SYMPTOM_TEXT, ignore.case = T) | grepl("failure to thrive", deaths$CUR_ILL, ignore.case = T) | grepl("failure to thrive", deaths$HISTORY, ignore.case = T), 1, deaths$hospice)
table(deaths$hospice)
deaths$cancer <- ifelse(grepl("cancer", deaths$SYMPTOM_TEXT, ignore.case = T) | grepl("cancer", deaths$CUR_ILL, ignore.case = T) | grepl("cancer", deaths$HISTORY, ignore.case = T), 1, 0)
table(deaths$cancer)

deaths <- deaths[!duplicated(deaths$VAERS_ID),]

ud %>%
  filter(NUMDAYS <= 7 | is.na(NUMDAYS)) -> ud1

deaths %>% 
  filter(suicide == 0 & hospice == 0) -> ud

  
nrow(ud)/171310000*100
# 0.002% of vaccinations were followed by death (no assumption of causality)

600000/33400000
# 1.8% of COVID-19 cases were followed by death

1.8/0.002

# 900 times more likely to die from COVID-19 than from the COVID-19 vaccine (U.S. case-fatality rate/vaccine adverse reported event rate)

ud %>%
  filter(AGE_YRS < 40 & AGE_YRS > 24) -> ud30s
107/31421854

ud %>%
  filter(AGE_YRS < 50 & AGE_YRS > 39) -> ud40s
147/22339951

# Number vaccinated by age 
25-34: 31





