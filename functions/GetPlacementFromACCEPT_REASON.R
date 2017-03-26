library(dplyr)
library(tidyverse)
#read in Cohort dataset 
library(readxl)
dat<- read_excel("~/Desktop/17 SPRING/Capstone /DHS_Case_Clients_2016EntryCohort.xlsx", sheet = "DHS_Case_Clients_2016EntryCohor")
dat2 <- dat %>% 
  select(CLIENT_ID,ACCEPT_REASON) %>% 
  mutate(isplaced=grepl("placed",ACCEPT_REASON)) %>% 
  group_by(CLIENT_ID) %>% 
  summarise(chplaced=sum(isplaced) %>% as.logical)
placeddat<-filter(dat2,chplaced==TRUE)
