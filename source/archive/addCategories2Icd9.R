rm(list = ls())

#load these packages:
packages <- c('rio', 'dplyr', 'icd')

for(package in packages){
  #INSTALL PACKAGES BEFORE RUNNING SCRIPT FOR FIRST TIME BY UNCOMMENTING LINE BELOW
  #install.packages(package) #if asked to restart, choose NO
  library(package, character.only = T)
}


#load indication -> icd9 crosswalk
xwalk.icd9 <-
  import('data/Cortellis_Drug_Indication_ICD9_Crosswalk.dta') %>% 
  filter(condition != 'unidentified indication') %>% 
  arrange(icd9)

xwalk.icd9$is_cancer <- sapply(xwalk.icd9$condition, is.cancer)

#replace these (incorrect) codings
xwalk.icd9$icd9[xwalk.icd9$icd9 > 999] <- NA

#use icd package to code as icd9
xwalk.icd9$icd9 <-
  xwalk.icd9$icd9 %>%
  as.character() %>%
  as.icd9()

#replace icd9s with character codings
xwalk.icd9$icd9 <- icd_short_to_decimal(xwalk.icd9$icd9)

#get condition labels
xwalk.icd9$icd9_category <- icd_explain_table(xwalk.icd9$icd9)$major
xwalk.icd9$icd9_sub_chapter <- icd_explain_table(xwalk.icd9$icd9)$sub_chapter
xwalk.icd9$icd9_chapter <- icd_explain_table(xwalk.icd9$icd9)$chapter

#select variables & sort
xwalk.icd9 <- 
  xwalk.icd9 %>%
  select(cortellis_condition = condition,
         is_cancer, 
         starts_with('icd9')
  ) %>% 
  arrange(as.numeric(icd9))

xwalk.icd9 %>%
  filter(is_cancer == 1 | icd9_chapter == 'Neoplasms') %>%
  mutate(revised_icd9 = NA) %>% 
  select(-is_cancer) %>% 
  select(cortellis_condition, revised_icd9, everything()) %>% 
  export('data/Cortellis_Drug_Indication_ICD9_Crosswalk_Cancer.csv')

xwalk.icd9 %>%
  filter(is_cancer != 1 & icd9_chapter != 'Neoplasms') %>%
  mutate(revised_icd9 = NA) %>% 
  select(-is_cancer) %>% 
  select(cortellis_condition, revised_icd9, everything()) %>% 
  export('data/Cortellis_Drug_Indication_ICD9_Crosswalk_Noncancer.csv')

#write to disk
export(xwalk.icd9, 'data/Cortellis_Drug_Indication_ICD9_Crosswalk_withCategories.csv')


