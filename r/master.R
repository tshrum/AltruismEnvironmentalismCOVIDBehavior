# Master File
# Created by Trisha R. Shrum
# Always run from this file. Run load.R, merge.R, and dataPrep.R before other scripts. Others are non-sequential.


#### Preliminary steps to set up the R environment ####
# Clear global environment
rm(list = ls()) 

# Load packages and custom functions
source('r/load.R', echo=TRUE)


#### Raw Data and De-identification ####
# Importing Raw Data
# Survey data collected via MTurk on the Qualtrics survey platform
# Survey used to collect data can be found here: 'data/Survey_Instrument_for_SEGS_COVID-19_Serious_Game_Part_I.qsf'
# Survey text can be found here: 'data/Survey_Instrument_for_SEGS_COVID-19_Serious_Game_Part_I_participantView.pdf'

# r <- read_csv("private_data/Survey+Instrument+for+SEGS+COVID-19+Serious+Game+(Part+I)_June+3,+2021_15.58.csv")

# Processing and Removing Location and IP Data
# The raw data file to run this script is not included because it has location based data.
# This script creates a csv file called rawData_noID.csv which includes all information collected except for IP Addresses and IP based location data beyond zip code and state

# source('r/deID.R', echo = TRUE)


#### Data Preparation ####
# Preparing raw data for analysis using de-identified and publicly available data 

r <- read_csv("data/UVM_SEGS_COVID19_Part1_rawData_noID.csv")
d <- r  # Preserving raw file in R environment as r, working data file is named d

# dataQuality.R uses data quality checks to remove test and incomplete surveys and flag low-quality data from the primary analysis. 
source('r/dataQuality.R', echo = TRUE)
# Relevant notes for quick access:
  # 633 responses (excluding flagged test responses from survey administrator)
  # 629 responses who consented to participate (4 participants dropped at the consent page)
  # 615 participants who completed at least half of the survey (14 participants dropped who did not complete at least half of the survey)
  # Identifying low-quality data with invalidResponse variable - exclude from analysis except for robustness checks (593 valid responses, 15 invalid)
  # Codes: 
  #   0 = valid response, 
  #   1 = likely bot (identified by Qualtrics), 
  #   2 = likely repeat (identified by duplicate IP and fast response time),
  #   3 = overly fast response (lowest 2.5% completion times)
  #   4 = reported mismatched zip codes and states

# fixZipState.R attempts to reconcile discrepancies in location data (region, state, zip)
# added invalidResponse: 3 = reported state and zip codes that did not match (checked first for typos)
source('r/fixZipState.R', echo = TRUE)

#3 Merge datasets from survey, COVID cases, and zip code based data
source('r/merge.R', echo=TRUE)

#4 Prepare data for analysis
source('r/dataPrep.R', echo=TRUE)
# Relevant notes for quick access:
  # Handling missing values or "unsure" responses
    # Social Risk Exposure index: # Converting "unsure" to an average of: 1) the sample mean for the activities and 2) the individual mean for all activities (excluding unsures)
  # Cronbach's alpha for indices:
    # NEP: alpha = 0.85
    # SRA: alpha
    # riskPerceptionBehaviorIndex: alpha = 0.94, scale(scale(d$socialPeople) + scale(d$socialFreq) + d$barsW + d$visitFriendsHouseW + d$hostVisitorsW + d$party10W + d$party50W + d$indoorDiningW + d$indoorGymW)
    # socialExposureIndex: alpha = 0.84
    # socialRiskReductionIndex: alpha = 0.75, scale(d$socialMask + d$socialDistancing)
    # riskReducingBehaviorIndex: alpha = 0.87
    # mentalHealthIndex: alpha = 0.78, d$mentalHealthIndex <- d$mentalNervous + d$mentalWorrying + d$mentalDepressed + d$mentalApathy + d$mentalLossControl + d$mentalProblems + d$mentalGoingYourWay + d$mentalOverwhelmed

# Analysis for NEP, Altruism, & COVID paper (under review at Environmental Health Perspectives)
source('r/analysis_ProEnviro.R', echo=TRUE)

# Summary stats of the data ***NEEDS TO BE UPDATED***
# source('r/summaryStats.R', echo=TRUE)

# Preliminary Analysis
#source('r/Analysis.R', echo=TRUE)

# Figures 
source('r/figures.R', echo=TRUE)




# Code Graveyard
# where code that was once useful goes to rest, likely for all eternity. Resurrection is possible.
#source('r/codeGraveyard.R', echo=TRUE)

# Preliminary Analysis
# Quick and dirty prep and analysis. Retired 11.11.2020
#source('r/prelimAnalysis.R', echo=TRUE)