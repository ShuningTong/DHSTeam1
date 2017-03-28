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

## Shuning
# from original data to mergedData
library(readxl)
dat1 <- read_excel("~/Desktop/Capstone/DHS_Case_Clients_2016EntryCohort.xlsx", 
                   sheet = "DHS_Case_Clients_2016EntryCohor") # 16639 obs
dat2 <- read_excel("~/Desktop/Capstone/DHS_CrossSystem.xlsx", 
                   sheet = "SystemInvolvement_EC2016") # 8206 obs
lengthunique<- function(x) {
  length(unique(x))
} 
ncase <- tapply(dat1$CASE_ID, dat1$CLIENT_ID, lengthunique)
table(ncase)
listOfClient <- sort(unique(dat1$CLIENT_ID))
multiCaseId <- which(ncase > 1) # 92
multiCaseClient <- listOfClient[multiCaseId]
multiCaseIdxCohort <- which(dat1$CLIENT_ID %in% multiCaseClient) # matches any of multiCaseClient
simplerData <- dat1[-multiCaseIdxCohort,]
length(unique(simplerData$CLIENT_ID))# 8774
dat1 <- simplerData
dat1Unique <- dat1[!duplicated(dat1$CLIENT_ID),]
dat1Unique <- dat1Unique[,-c(14, 15)] # 8774 obs
mergedData <- merge(x = dat1Unique, y = dat2, by = "CLIENT_ID") # inner join 8084 obs

# isPlaced
library("dplyr")
selected <- select(mergedData, CLIENT_ID, CASE_ID, ACCEPT_REASON)
mutated <- mutate(selected, isplaced = grepl("placed",ACCEPT_REASON))
# enough to get isPlaced

# using %>%
placeData <- mergedData %>% 
  select(CLIENT_ID, CASE_ID, ACCEPT_REASON) %>% 
  mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
  group_by(CASE_ID) %>% 
  summarise(chplaced=sum(isplaced) %>% as.logical)


### test different ways of writing same codes
install.packages("nycflights13")
library(nycflights13)
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)