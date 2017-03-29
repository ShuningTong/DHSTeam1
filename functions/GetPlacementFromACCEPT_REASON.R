library(dplyr)
library(tidyverse)
#read in Cohort dataset 
library(readxl)
dat<- read_excel("~/Desktop/Capstone/DHS_Case_Clients_2016EntryCohort.xlsx", sheet = "DHS_Case_Clients_2016EntryCohor")
dat2 <- dat %>% 
  select(CLIENT_ID,ACCEPT_REASON) %>% 
  mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
  group_by(CLIENT_ID) %>% 
  summarise(chplaced=sum(isplaced) %>% as.logical)
placeddat<-filter(dat2,chplaced==TRUE)

### Shuning's Comment to Ziyi's Code
# group_by 应该是CASE_ID，而不是CLIENT_ID
# 所以数据应该用mergedData
# 最后不用filter出TRUE的值

### Shuning's Code
library(dplyr)
# from original data to mergedData
lengthunique<- function(x) {
  length(unique(x))
} 
mergeData <- function(entrycohort, crosssystem){
  listOfClient <- sort(unique(entrycohort$CLIENT_ID))
  ncase <- tapply(entrycohort$CASE_ID, entrycohort$CLIENT_ID, lengthunique)
  multiCaseId <- which(ncase > 1)
  multiCaseClient <- listOfClient[multiCaseId]
  multiCaseIdCohort <- which(entrycohort$CLIENT_ID %in% multiCaseClient) # matches any of multiCaseClient
  entrycohort <- entrycohort[-multiCaseIdCohort,]
  entrycohortUnique <- entrycohort[!duplicated(entrycohort$CLIENT_ID),] #扔掉同一个CLIENT_ID的多条记录
  entrycohortUnique <- entrycohortUnique[,-c(14, 15)] #扔掉CLOSE_DT和CLOSE_REASON
  mergedData <- merge(x = entrycohortUnique, y = crosssystem, by = "CLIENT_ID") # inner join
  return(mergedData)
}
# get placement data
getPlaceData <- function(mergedData){
  placeData <- mergedData %>% 
    select(CLIENT_ID, CASE_ID, ACCEPT_REASON) %>% 
    mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
    group_by(CASE_ID) %>% 
    summarise(isPlacedFromAcceptReason=sum(isplaced) %>% as.logical)
  return(placeData)
}

### how to call these functions
# library(readxl)
# dat1 <- read_excel("~/Desktop/Capstone/DHS_Case_Clients_2016EntryCohort.xlsx", 
#                    sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
# dat2 <- read_excel("~/Desktop/Capstone/DHS_CrossSystem.xlsx", 
#                    sheet = "SystemInvolvement_EC2016") # 8206 obs
# mergedData <- mergeData(dat1, dat2) # 8084 obs
placeData <- getPlaceData(mergedData)



### different ways of writing same codes
### understand chaining and the usage of %>%
# install.packages("nycflights13")
# library(nycflights13)
# a1 <- group_by(flights, year, month, day)
# a2 <- select(a1, arr_delay, dep_delay)
# a3 <- summarise(a2,
#                 arr = mean(arr_delay, na.rm = TRUE),
#                 dep = mean(dep_delay, na.rm = TRUE))
# a4 <- filter(a3, arr > 30 | dep > 30)
# 
# filter(
#   summarise(
#     select(
#       group_by(flights, year, month, day),
#       arr_delay, dep_delay
#     ),
#     arr = mean(arr_delay, na.rm = TRUE),
#     dep = mean(dep_delay, na.rm = TRUE)
#   ),
#   arr > 30 | dep > 30
# )
# 
# flights %>%
#   group_by(year, month, day) %>%
#   select(arr_delay, dep_delay) %>%
#   summarise(
#     arr = mean(arr_delay, na.rm = TRUE),
#     dep = mean(dep_delay, na.rm = TRUE)
#   ) %>%
#   filter(arr > 30 | dep > 30)