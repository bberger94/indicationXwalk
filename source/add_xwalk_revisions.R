rm(list = ls())

#Load these packages:
packages <- c('rio', 'dplyr', 'icd')
for(package in packages){
  #INSTALL PACKAGES BEFORE RUNNING SCRIPT FOR FIRST TIME BY UNCOMMENTING LINE BELOW
  #install.packages(package) #if asked to restart, choose NO
  library(package, character.only = T)
}

#Load indication -> icd9 crosswalk
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

#Load revised matches
xwalk.icd9 <- 
  import('data/validatedInd2ICD/cancer/Cortellis_Drug_Indication_ICD9_Crosswalk_Cancer_ben.xlsx') %>% 
  select(condition = cortellis_condition,
         revised_icd9 = revised_icd9_ben,
         malignant_not_specified) %>% 
  right_join(xwalk.icd9, by = 'condition')

#Replace ICD-9s with revised ICD-9s
xwalk.icd9$icd9[!is.na(xwalk.icd9$revised_icd9)] <-
  xwalk.icd9$revised_icd9[!is.na(xwalk.icd9$revised_icd9)]

#Replace ICD-9s where there is no appropriate match with NA
xwalk.icd9$icd9[xwalk.icd9$revised_icd9 == 'No appropriate ICD-9'] <- NA

#Replace revised icd9s with character codings
xwalk.icd9$icd9 <- icd_short_to_decimal(xwalk.icd9$icd9)

#Replace malignant_not_specified with 0 if NA
xwalk.icd9$malignant_not_specified[is.na(xwalk.icd9$malignant_not_specified)] <- 0

