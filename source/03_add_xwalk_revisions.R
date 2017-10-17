## ------------------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------------------ ##
## 03_add_xwalk_revisions.R ; Author: Ben Berger;                               
##
## Revises indication -> icd9 mappings to "Cortellis_Drug_Indication_ICD9_Crosswalk.dta"
## (from C. Garthwaite)
## 
## Also adds new mappings for Cortellis indications in trial data that do not appare in the 
## original crosswalk.
##
## ------------------------------------------------------------------------------------------------ ##
## ------------------------------------------------------------------------------------------------ ##

rm(list = ls())

#Load these packages:
packages <- c('rio', 'dplyr', 'icd')
for(package in packages){
  #INSTALL PACKAGES BEFORE RUNNING SCRIPT FOR FIRST TIME BY UNCOMMENTING LINE BELOW
  #install.packages(package) #if asked to restart, choose NO
  library(package, character.only = T)
}



## ------------------------------------------------------------------------------------------------ ##
##  Load indication -> icd9 crosswalk (from C. Garthwaite)
## ------------------------------------------------------------------------------------------------ ##

xwalk.icd9 <-
  import('data/Cortellis_Drug_Indication_ICD9_Crosswalk.dta') %>% 
  filter(condition != 'unidentified indication') %>% 
  arrange(icd9)


#Replace these (incorrect) codings
xwalk.icd9$icd9[xwalk.icd9$icd9 > 999] <- NA

#Use icd package to code as icd9
xwalk.icd9$icd9 <-
  xwalk.icd9$icd9 %>%
  as.character() %>%
  as.icd9()

#Replace icd9s with character codings
xwalk.icd9$icd9 <- icd_short_to_decimal(xwalk.icd9$icd9)

# Load new indications (added 10-17-17)
new_indications <- 
  import('data/validatedInd2ICD/missing_inds/Cortellis_Drug_Indication_ICD9_Crosswalk_missing_inds_ben.xlsx') %>% 
  select(condition = indication_name, icd9 = icd9_ben) %>% 
  mutate(icd9 = icd_short_to_decimal(as.icd9(icd9)))

xwalk.icd9 <- bind_rows(xwalk.icd9, new_indications)


## ------------------------------------------------------------------------------------------------ ##
##  Load revision files
##  Revision files with names ending in "ben" were adjudicated for discrepancies by Ben
##  from mappings performed by Jackie, Robbie, and Holly
## ------------------------------------------------------------------------------------------------ ##

# Import adjudicated revision files and select key variables
revisions_cancer <- 
  import('data/validatedInd2ICD/cancer/Cortellis_Drug_Indication_ICD9_Crosswalk_Cancer_ben.xlsx') %>% 
  select(condition = cortellis_condition,
         revised_icd9 = revised_icd9_ben,
         malignant_not_specified) 

revisions_noncancer <- 
  import('data/validatedInd2ICD/noncancer/Cortellis_Drug_Indication_ICD9_Crosswalk_Noncancer_ben.xlsx') %>% 
  select(condition = cortellis_condition,
       revised_icd9 = revised_icd9_ben
       ) 
# Note: These revisions were inadvertently omitted from the original round of revisions
revisions_extra <- 
  import('data/validatedInd2ICD/noncancer/Cortellis_Drug_Indication_ICD9_Crosswalk_Extra_Ind_ben.xlsx') %>% 
  select(condition = cortellis_condition,
         revised_icd9 = revised_icd9_ben
  ) 

# Bind revisions together
revisions <- bind_rows(revisions_cancer, revisions_noncancer, revisions_extra)

# Merge all revisions into crosswalk
xwalk.icd9 <- 
  revisions %>% 
  right_join(xwalk.icd9, by = 'condition')

#Replace ICD-9s with revised ICD-9s
xwalk.icd9$icd9[!is.na(xwalk.icd9$revised_icd9)] <-
  xwalk.icd9$revised_icd9[!is.na(xwalk.icd9$revised_icd9)]

#Replace ICD-9s where there is no appropriate match with NA
xwalk.icd9$icd9[xwalk.icd9$revised_icd9 == 'No appropriate ICD-9'] <- NA

#Drop revised ICD-9 variable
xwalk.icd9 <-
  xwalk.icd9 %>% select(-revised_icd9)

#Replace icd9s with character codings
xwalk.icd9$icd9 <- icd_short_to_decimal(xwalk.icd9$icd9)


## ------------------------------------------------------------------------------------------------ ##
##  Add ICD-9 hierarchy
## ------------------------------------------------------------------------------------------------ ##

xwalk.icd9$icd9_category <- icd_explain_table(xwalk.icd9$icd9)$major
xwalk.icd9$icd9_sub_chapter <- icd_explain_table(xwalk.icd9$icd9)$sub_chapter
xwalk.icd9$icd9_chapter <- icd_explain_table(xwalk.icd9$icd9)$chapter


## ------------------------------------------------------------------------------------------------ ##
##  Write to file
## ------------------------------------------------------------------------------------------------ ##

#select variables & sort
xwalk.icd9 <- 
  xwalk.icd9 %>%
  select(cortellis_condition = condition,
         starts_with('icd9'),
         malignant_not_specified
  ) %>% 
  arrange(icd9)

xwalk.icd9 %>%
  export('data/Cortellis_Drug_Indication_ICD9_Crosswalk_Validated_10-17-17.csv')


