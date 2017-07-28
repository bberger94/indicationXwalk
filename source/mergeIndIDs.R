rm(list = ls())

library(rio)
library(dplyr)

#load approvals
data <- import('data/dh_drugapproval_20062017_BAB_06152017.xlsx')
data$indication <- tolower(data$indication)
nrow(data)

#load xwalk: cortellis indication -> cortellis indication ID
IDxwalk <- import('data/cortellisInd2IndID.csv')
colnames(IDxwalk) <- c('indicationID', 'indication')
IDxwalk$indication <- tolower(IDxwalk$indication)

#merge in indication IDs
merged <- data %>% left_join(IDxwalk, by = 'indication')
tally.nomatch <- sum(is.na(merged$indicationID))
cat(tally.nomatch, "IDs did NOT match")

export(merged, "data/drugApproval_tempfile.Rdata")


####################keep working on this on the 'morrow
count.na <- function(x) count <- sum(is.na(x) == FALSE) 
blankstring.2.na <- function(x){
  x[x == ""] <- NA
  return(x)
}


indMapping <- import('data/indication_mappings.csv')
colnames(indMapping)[colnames(indMapping)=='ciIndication'] <- 'indicationID'
indMapping <- indMapping %>% mutate_each(funs(blankstring.2.na))

#indicate which cortellis IDs correspond to multiple rows in mapping
duplicatedIDs <- unique(indMapping$indicationID[duplicated(indMapping$indicationID)])
indMapping$duplicated <- 0
indMapping$duplicated[is.element(indMapping$indicationID, duplicatedIDs)] <- 1



merged <- data %>% left_join(IDxwalk, by = 'indication') %>%  
                   left_join(indMapping, by = 'indicationID') %>% 
                   arrange(brandname)


##SUMMARY STATS
testdata <- merged[, c('drugname','brandname','indication', colnames(indMapping))] 

#number of drugs which correpond to multiple rows in mapping
length(unique(testdata$drugname[testdata$duplicated == 1]))
#total number of drugs
length(unique(testdata$drugname))
#percent of drugs that map to multiple entries 
length(unique(testdata$drugname[testdata$duplicated == 1])) / length(unique(testdata$drugname))

grouped <- testdata %>% group_by(drugname) %>% summarize(count = n())
pie(table(grouped$count))




sum(duplicated(testdata$drugname))/length(unique(testdata$drugname)) 


apply(testdata, 2, count.na)





