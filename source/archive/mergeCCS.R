rm(list = ls())
packages <- c('rio', 'dplyr')

for(package in packages){
#INSTALL PACKAGES BEFORE RUNNING SCRIPT FOR FIRST TIME BY UNCOMMENTING LINE BELOW
  #install.packages(package) #if asked to restart, choose NO
  library(package, character.only = T)
}

#HELPER FUNCTIONS
#Get statistical mode of a vector
get.mode <- function(x){
  if(all(is.na(x))) return(NA)
  else names(sort(table(x), decreasing = TRUE))[1]
}

#Test equality of elements along 2 vectors, including NAs
test.equality <- function(x, y){
  l <- rep(0, length(x))
  for(i in 1:length(x)){
    if(is.na(x[i]) == FALSE & is.na(y[i]) == FALSE) l[i] <- as.numeric(x[i] == y[i])
    else if(sum(is.na(x[i]), is.na(y[i])) == 2) l[i]  <- 1
    else if(sum(is.na(x[i]), is.na(y[i])) == 1) l[i]  <- 0
  }
  return(l)
}

#Remove quotes and any leading/ending whitespace from strings
clean.strings <- function(x){
  x <- gsub("'", "", x)
  x <- trimws(x)
  x
}

#LOAD DATA INTO MEMORY
  #approvals
  data <-
    import('data/drugApproval_tempfile.Rda') %>%
    #select(drugname, brandname, breakthrough, year, indication) %>% 
    filter(year >= 2006)
  
  #craig's 3 digit ICD9 crosswalk
  xwalk.icd9 <- import('data/Cortellis_Drug_Indication_ICD9_Crosswalk.dta')
  #HCUP CCS crosswalk & documentation
  xwalk.ccs <- import('data/ccs/$dxref 2015.csv')
  labels.ccs <- import('data/ccs/dxlabel 2015.csv')

#PREPARE DATA
  #clean icd9 xwalk
  xwalk.icd9 <-
    xwalk.icd9 %>%
    arrange(icd9) %>%
    rename(indication = condition) 
  
  #manually change some ICD9s
  xwalk.icd9$icd9[xwalk.icd9$indication == 'papillomavirus infection'] <- 79
  xwalk.icd9$icd9[xwalk.icd9$indication == 'hiv-1 infection' | 
                  xwalk.icd9$indication == 'hiv-2 infection' ] <- 42
  
  #clean ccs xwalk
  xwalk.ccs <- xwalk.ccs[-1,] #remove first row
  colnames(xwalk.ccs) <- clean.strings(colnames(xwalk.ccs))
  colnames(xwalk.ccs)[2] <- 'ccs'
  
  xwalk.ccs[] <- apply(xwalk.ccs, 2, clean.strings)
  xwalk.ccs$icd9 <- substr(c(xwalk.ccs$'ICD-9-CM CODE'), 1, 3) %>% as.numeric()

#CREATE COMPOSITE CROSSWALK
  #merge xwalks into one xwalk with multiple ICD9s for each 3 digit grouping
  xwalk.merged <- 
    xwalk.icd9 %>% 
    left_join(xwalk.ccs, by = 'icd9') %>% 
    arrange(icd9) %>% 
    select(indication, icd9, ccs, ccs_desc = `CCS CATEGORY DESCRIPTION`, icd9_4d = `ICD-9-CM CODE`, icd9_4d_desc = `ICD-9-CM CODE DESCRIPTION`) 
  
  #identify number of unique CCS codes per 3-digit icd9 grouping
  xwalk.merged <- 
    xwalk.merged %>%
    group_by(icd9) %>%
    mutate(nccs = length(unique(ccs)[!is.na(unique(ccs))]))
  
  #bind observations into one crosswalk
  xwalk.final <-
    xwalk.merged %>%
    group_by(indication) %>% 
    mutate(ccs.modal = get.mode(ccs)) %>%
    filter(test.equality(ccs.modal, ccs) == 1) %>% 
    mutate(tempindx = row_number()) %>%
    filter(tempindx == 1) %>% 
    left_join(labels.ccs, by = c('ccs' = 'CCS DIAGNOSIS CATEGORIES')) %>% 
    select(indication, icd9, ccs, ccs_label = `CCS DIAGNOSIS CATEGORIES LABELS`, nccs)

  #clean crosswalk
  xwalk.final$ccs <- as.numeric(xwalk.final$ccs)
  xwalk.final$cancer <- as.numeric(xwalk.final$ccs >= 11 & xwalk.final$ccs <= 44)
  xwalk.final <- xwalk.final %>% arrange(ccs)

#MERGE CCS CODES INTO DATA
merged <- 
  data %>% 
  left_join(xwalk.final, by = 'indication') %>% 
  arrange(icd9)

#WRITE TO DISK
export(xwalk.final, 'data/CortellisIndication_CCS_xwalk.csv')
export(merged, 'data/drugapproval_mergedCCS.csv')


#MAKE SUMMARY STATISTICS
#count number of conditions that link to icd9s with 2+ CCS codes

#IN CROSSWALK
nInd2plus <-
  xwalk.merged %>%
  filter(nccs >= 2) %>% 
  group_by(indication) %>% 
  summarize(min(nccs)) %>% 
  nrow()
nICD2plus <-
  xwalk.merged %>%
  filter(nccs >= 2) %>% 
  group_by(icd9) %>% 
  summarize(min(nccs)) %>% 
  nrow()
cat('In the crosswalk,' , 
    nInd2plus, 'Cortellis indications correspond to',
    nICD2plus, '3-digit ICD9s with 2+ CCS codes')

#IN APPROVAL DATA
nInd2plus <-
  merged %>%
  filter(nccs >= 2) %>% 
  group_by(indication) %>% 
  summarize(min(nccs)) %>% 
  nrow()
nICD2plus <-
  merged %>%
  filter(nccs >= 2) %>% 
  group_by(icd9) %>% 
  summarize(min(nccs)) %>% 
  nrow()
cat('In the approval data,' , 
    nInd2plus, 'Cortellis indications correspond to',
    nICD2plus, '3-digit ICD9s with 2+ CCS codes')



#count number of conditions that have word cancer in them

#HELPER FUNCTION!
#Counts the number of elements of a vector with a substring matching at least one of the specified terms
count.strings <- function(terms, x){
  x <- tolower(x)
  terms <- tolower(terms)
  v <- rep(0, length(x))
  for(term in terms){
    v.term <- grepl(term, x)
    v[v == 0] <- v.term[v == 0]
  }
  sum(v)
}

#IN CROSSWALK
indications <- xwalk.final$indication

count.strings('cancer', indications)
count.strings(c('cancer', 'melanoma', 'sarcoma', 'carcinoma'), indications)
count.strings(c('cancer', 'melanoma', 'sarcoma', 'carcinoma', 'tumor'), indications)

cancer <- xwalk.final$cancer
sum(cancer, na.rm = T)

#IN DATA
indications <- merged$indication
count.strings('cancer', indications)
count.strings(c('cancer', 'melanoma', 'sarcoma', 'carcinoma'), indications)
count.strings(c('cancer', 'melanoma', 'sarcoma', 'carcinoma', 'tumor'), indications)

cancer <- merged$cancer
sum(cancer, na.rm = T)







