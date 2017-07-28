#get condition labels
xwalk.icd9$icd9_category <- icd_explain_table(xwalk.icd9$icd9)$major
xwalk.icd9$icd9_sub_chapter <- icd_explain_table(xwalk.icd9$icd9)$sub_chapter
xwalk.icd9$icd9_chapter <- icd_explain_table(xwalk.icd9$icd9)$chapter

#select variables & sort
xwalk.icd9 <- 
  xwalk.icd9 %>%
  select(cortellis_condition = condition,
         starts_with('icd9'),
         malignant_not_specified
  ) %>% 
  arrange(as.numeric(icd9))

xwalk.icd9 %>%
  export('data/Cortellis_Drug_Indication_ICD9_Crosswalk_cancerValidated.csv')
