rm(list = ls())

library(rio)
library(dplyr)
library(ggplot2)
library(tidyr)

#load approvals
data <- import('data/drugApproval_tempfile.Rda')
#craig's 3 digit ICD9 crosswalk
xwalk <- import('data/Cortellis_Drug_Indication_ICD9_Crosswalk.dta')
atcxwalk <- import('data/fwcrosswalkfile/icd9_atc2_xwalk.dta')

#rename merging variables
xwalk <- xwalk %>% arrange(icd9) %>% rename(indication = condition) 
atcxwalk <- atcxwalk %>% rename(icd9 = Indications, atc = atc2_mostcommon_icd9)

#merge using pipes because pipes are dope
merged <-
  data %>%
  left_join(xwalk, by = 'indication') %>%
  arrange(icd9)

#count number of drugs without matching ICD9s
merged$icd9 %>% is.na %>% sum

#count number of bkthru drugs without matching ICD9s
filter(merged, breakthrough == 1)$icd9 %>% is.na %>% sum


merged <-
  merged %>% 
  left_join(atcxwalk, by = 'icd9') %>% 
  arrange(atc)


#count number of drugs without matching ATC
merged$atc %>% is.na %>% sum

#count number of bkthru drugs without matching ATCs
filter(merged, breakthrough == 1)$atc %>% is.na %>% sum

#make candidate data
merged %>%
  select(drugname, brandname, breakthrough,year, atc, icd9, indication) %>%
  arrange(atc, icd9, indication, year, drugname) %>% 
  export(file = 'data/drugapprovals_withATC.csv')


