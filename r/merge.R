# Merging data sets prior to data prep and identifying and correcting geographic discrepancies
# Created by: Trisha R. Shrum

load(file = 'data/workingDataset_dataQuality_fixZipState.RData')

#### Loading Data ####
# All of these datasets are publicly available. Links provided where retrieved. Datafiles are stored in the data directory.

# URL: https://www.hrsa.gov/rural-health/about-us/definition/datafiles.html
# Specific Data File link: https://www.hrsa.gov/sites/default/files/hrsa/ruralhealth/aboutus/definition/forhp-zips-by-counties.xlsx
# Description: This file contains 25,964 records by State, County and ZIP with the percentage of the ZIP codes’ population that resides in that county (not census tract), the FIPS code for the county and its OMB designation. Some ZIPs are for businesses or PO boxes or have unknown population totals. These are marked by a “U” in the population field. ZIP Codes can include area and population in more than one county so there can be multiple records for one ZIP Code in this file.
rural <- read_csv("data/forhp-zips-by-counties.csv", col_types = cols(.default = "c"))  
    
# URL: https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36 
cases <- read_csv("data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")  

# URL: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html
# Description: state population
p <- read_csv("/Users/tshrum/Projects/SEGS_COVID/data/statePop.csv") 

# URL: https://doi.org/10.1073/pnas.1317937111
# Description: Table from Harrington, J. R., & Gelfand, M. J. (2014). Tightness–looseness across the 50 united states. Proceedings of the National Academy of Sciences, 111(22), 7990–7995. 
t <- read_csv("/Users/tshrum/Projects/SEGS_COVID/data/Harrington_and_Gelfand_2014_Table_1.csv") 

# URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
# Description: State level presidential election vote totals
v <- read_csv("/Users/tshrum/Projects/SEGS_COVID/data/dataverse_files/1976-2020-president.csv")  


#### Rural  ####
# rural = 1: Federal Office of Rural Health Policy eligible zip code, rural = 0: Not designated as rural
d %>%
  dplyr::select(ResponseId, zip) -> z

rural %>%
  mutate(zip = ZIP_CODE, rural = `CBSA2020 Update`, fipsCounty = STCountyFIPS, county = coname) %>%
  dplyr::select(zip, rural, county, fipsCounty) -> rural

left_join(z, rural, by = "zip") -> z
z[!duplicated(z$ResponseId),] -> z

z$rural <- ifelse(is.na(z$rural), 0, 1)

z %>%
  dplyr::select(ResponseId, rural) %>%
  full_join(d, by = "ResponseId") -> d


#### State COVID cases ####
# Merging state covid case data (dataframe "cases") with d dataset

# standardizing date formats
d$date <- substring(d$StartDate, 1, 10)
d$date <- as.Date(d$date, "%Y-%m-%d")
cases$date <- as.Date(cases$submission_date, "%m/%d/%Y")

# adding up cases and deaths in the past week for a given submission date in a given state
weeklyCases <- function(date, state) {
  which(cases$date == date & cases$state == state) -> i
  cases$new_case[i-1] + cases$new_case[i-2] + cases$new_case[i-3] + cases$new_case[i-4] +
    cases$new_case[i-5] + cases$new_case[i-6] + cases$new_case[i-7]
}
weeklyDeaths <- function(date, state) {
  which(cases$date == date & cases$state == state) -> i
  cases$new_death[i-1] + cases$new_death[i-2] + cases$new_death[i-3] + cases$new_death[i-4] +
    cases$new_death[i-5] + cases$new_death[i-6] + cases$new_death[i-7]
}

d$pastWeekCases <- NA
for (i in 1:nrow(d)) {
  if(!is.na(d$state[i]) & !(is.na(d$date[i]))) {
    weeklyCases(d$date[i], d$state[i]) -> d$pastWeekCases[i]
  }
}
d$pastWeekDeaths <- NA
for (i in 1:nrow(d)) {
  if(!is.na(d$state[i]) & !(is.na(d$date[i]))) {
  weeklyDeaths(d$date[i], d$state[i]) -> d$pastWeekDeaths[i]
  }
}

#### State Population Data ####
# Pulling in state population data to bring COVID case numbers to per capita rates
p$state <- str_replace(p$state, ".", "")
p[!is.na(p$state),] -> p 
p$state <- state.abb[match(p$state,state.name)]
p$state[9] <- "DC"
p$state[52] <- "PR"
left_join(d, p, by = "state") -> d
d$pastWeekCasesPC <- d$pastWeekCases/d$pop*1000
d$pastWeekDeathsPC <- d$pastWeekDeaths/d$pop*1000

#### Tightness Score ####
# Merging Tightness scores by State
t$state <- state.abb[match(t$State,state.name)]
t$tightness <- t$Score
t %>%
  dplyr::select(state, tightness) %>%
  right_join(d, by = "state") -> d
sum(is.na(d$state))
sum(is.na(d$tightness))

#### Voting Data ####
# Adding in state level voting data
v %>%
  filter(year == 2020) %>%
  filter(candidate == "TRUMP, DONALD J.") %>%
  mutate(trumpPercent = candidatevotes/totalvotes) %>%
  mutate(state = state_po) -> v
v %>%
  dplyr::select(state, trumpPercent) %>%
  right_join(d, by = "state") -> d
sum(is.na(d$trumpPercent))

# Demographic info for PO Box zip codes aren't available. 
# replacing PO Box zip codes with closest non-PO Box zip codes -- most are "contained within" the substitute zip code according to https://www.hometownlocator.com/
d$zip <- decode(d$zip,
                    search = c("07097", "19101", "28201", "30212", "30301", "47870", "68738", "80201", "85001", "90009", "90622", "94016", "92513", "95009", "98114"),
                    replace = c("07307", "19104", "28217", "30223", "30303", "47802", "68701", "80205", "85004", "90045", "90620", "94112", "92503", "95008", "98104"))

# zipcodeR package
# Creating population density variable from zip code 
d$popDensity <- NA
for (i in 1:nrow(d)) {
  if (!is.na(d$zip[i])) {
    d$popDensity[i] <- reverse_zipcode(d$zip[i])$population_density
  }
  if (is.na(d$zip[i])) {
    d$popDensity[i] <- NA
  }
}
sum(is.na(d$popDensity))  # 7 NA, no additional dropped data

save(d, file = 'data/workingDataset_dataQuality_fixZipState_merged.RData')
rm(cases, p, rural, t, v, z)

