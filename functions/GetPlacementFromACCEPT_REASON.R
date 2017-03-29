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
# group_by should be CASE_ID，not CLIENT_ID
# data should be mergedData (drop those clients involved in more than one case)
# no need to filter TRUE values