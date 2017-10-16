## ------------------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------------------ ##
## save_missing_inds.R ; Author: Ben Berger;                               
## Modified from script by Andrew Marder:                              
##
## Obtain indications in trial data that are NOT PRESENT in ICD-9 crosswalk
##
## ------------------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------------------ ##

## Load packages 
library(dplyr) 
library(rlang)
library(readr)
library(tidyr)
library(haven)

## ------------------------------------------------------------------------------------------------ ##
##  Load data
## ------------------------------------------------------------------------------------------------ ##

## Read in data
options(stringsAsFactors = FALSE)

# Uncomment line below to load data without building from scratch
load('../clinical_trials/data/long_data.RData')

# Load crosswalk
icd9_xwalk <- read_csv('../clinical_trials/data/Cortellis_Drug_Indication_ICD9_Crosswalk_Validated.csv')

## ------------------------------------------------------------------------------------------------ ##
##  1. Merge indications onto crosswalk
##  2. Pick indications with missing ICD-9s 
##  3. Count number of trials using each indication
##  4. Write to file
## ------------------------------------------------------------------------------------------------ ##

missing_inds <- 
  icd9_xwalk %>% 
  rename(indication_name = cortellis_condition) %>% 
  right_join(indications_long) %>% 
  filter(is.na(icd9)) %>% 
  group_by(indication_name) %>% 
  summarize(trial_count = n()) 


write_csv(missing_inds, path = 'data/missing_inds.csv')
