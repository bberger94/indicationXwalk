rm(list = ls())

library(rio)
library(dplyr)

data <- import('data/drugapproval_tempfile.dta')
data <- data[-nrow(data),] #remove last row
data <- data %>% unique() %>% arrange(indication)

xwalk <- import('data/fwcrosswalkfile/icd9_atc2_xwalk.dta')
colnames(xwalk)[1] <- 'icd9'

merged <- data %>% left_join(xwalk, by = 'icd9') %>% rename(atc2 = atc2_mostcommon_icd9) %>% arrange(atc2)



export(merged, 'data/atc2_groups.csv')
